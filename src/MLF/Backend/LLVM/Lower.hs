{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.LLVM.Lower
Description : Lower typed backend IR into real LLVM IR syntax
-}

{- Note [One backend IR lowering boundary]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xMLF remains the thesis-faithful typed elaboration IR, and `MLF.Backend.IR`
is the single executable eager backend IR. Both 'lowerBackendProgram' and
'lowerBackendProgramNative' lower the same `MLF.Backend.IR` program.
LLVM lowering and native emission own only the downstream private
lowering/runtime details for that program: closure ABI details,
environment-record layout, layout-only lowering helpers, native
wrapper/runtime symbol emission, and executable rendering support. Those
details do not create a second executable IR, and they do not introduce a
lazy runtime. There are no thunks, no update frames, no CAF update semantics,
no graph reduction, and no implicit laziness rescue in this lowering layer.

Any ANF-like normalization, layout-only structure, or lowerability-only
representation in this module stays private to backend-owned lowering helpers
rather than becoming a second executable IR, a public `LowerableBackend.IR`,
or a second checked-program authority.

A later lower IR may be introduced only when all of the following hold:

* distinct backend-owned executable invariants that cannot live in
  `MLF.Backend.IR` or a private lowering helper;
* a dedicated validation/evidence owner for that new boundary; and
* a later accepted roadmap revision before any new durable or public surface
  is added.
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
the explicit `BackendClosureCall` node. Lowering consumes that same callable
contract rather than legalizing malformed `BackendApp` heads after let/case
peeling. `BackendApp` remains the direct first-order call path, and closure-
valued aliases, captured closures, and case/let-selected closure values must
already reach lowering as `BackendClosureCall`. Raw LLVM emission and native emission
both start from the same `MLF.Backend.IR` program; the private
`MLF.Backend.CallableShape` owner supplies the shared direct-vs-closure head
classifier. The closure ABI, native wrapper/runtime symbol emission, and
executable rendering support stay downstream of that IR rather than becoming a
second executable IR or a lazy runtime.
-}
{- Note [ADT constructor runtime layout]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Row-4 ADT/case ownership keeps semantic constructor/case nodes in
`MLF.Backend.IR`; runtime tags, field slots, closure-record storage for
function-like fields, and nullary tag-only representation stay private to
LLVM/native lowering.

The current private lowerer-owned policy is intentionally small and frozen by
focused tests:

* declaration-order zero-based constructor tags are assigned by
  `constructorRuntimes`;
* the tag word is stored at object offset `0`;
* field slots start after that tag word, one machine word per constructor
  field;
* function-like constructor fields are stored as explicit closure records
  using the private closure ABI; and
* nullary constructors use tag-only heap objects.

These layout facts are not a second executable IR or public lowering surface.
Checked-program conversion and `MLF.Backend.IR` keep only semantic ADT/case
metadata and term nodes.
-}
{- Note [Primitive operations and eager sequencing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Row-5 primitive/eager ownership keeps the primitive surface at the
inventory-owned reserved runtime-binding set in `MLF.Primitive.Inventory`:
`__mlfp_and`, `__string_length`, `__string_is_empty`,
`__string_contains_char`, `__string_contains`, `__string_equals`, `__string_starts_with`,
`__string_ends_with`, `__string_append`, `__string_from_char`,
`__string_replace_char`, `__string_replace`, `__string_from_int`, `__string_from_bool`,
`__string_index_of_char`, `__string_index_of`, `__string_split`,
`__string_from_nat`, `__string_to_list`, `__string_drop`, `__string_take`,
`__string_slice`, `__string_char_at`, `__string_char_at_option`,
`__char_is_digit`, `__char_is_ascii_lower`, `__char_is_ascii_upper`,
`__char_is_ascii_alpha`,
`__char_is_ascii_alpha_num`, `__char_is_ascii_identifier_start`,
`__char_is_ascii_identifier_continue`, `__char_is_ascii_whitespace`,
`__char_is_ascii_punctuation`, `__char_is_ascii_printable`, plus the IO
primitive names classified there for native support.
Those primitives still arrive through the existing `BackendVar`, `BackendApp`, and `BackendTyApp` surface, with no new `BackendPrim`, no broad FFI surface, and no fallback runtime executor hidden inside lowering.

The lowerer relies on the current eager order exactly as written:

* let RHS before body: `lowerExpr` calls `bindLet` before lowering the body;
* case scrutinee before branch selection: `lowerHeapCase` lowers the scrutinee before reading the tag and entering an alternative;
* direct/primitive call arguments in written order: direct call sites use `zipWithM`, and primitive/global primitive sites use `traverse` on the written argument list; and
* effect sequencing remains explicit through `__io_bind`.

Unsupported broader primitive or ordering-sensitive shapes still fail with
explicit backend diagnostics instead of widening this boundary.
-}
{- Note [Polymorphism erasure and lowerability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Row-6 keeps checked `Backend.IR` permissive while keeping emitted executables
narrow:

* checked `Backend.IR` may still carry `BackendTyAbs` and `BackendTyApp`.
* LLVM/native lowering owns only the specialization-based lowerable subset.
* Complete type applications may specialize privately inside the lowerer.
* Residual runtime polymorphism remains unsupported and must fail with explicit diagnostics without widening the backend boundary.

`collectRequiredSpecializations`, `lowerTyApp`, and `lowerGlobalValue` keep the
current static specialization lane alive for complete type applications and
other fully instantiated callable paths. `lowerExpr` rejects escaping
`BackendTyAbs` values, `resolveTypeArguments` rejects partial type
applications, `lowerFunction` rejects unspecialized polymorphic functions that
would otherwise be emitted directly, and `lowerBackendProgramNative` together
with `nativeRenderableKind` reject polymorphic `main` bindings or result
shapes before native emission.
-}
module MLF.Backend.LLVM.Lower
  ( BackendLLVMError (..),
    evidenceFunctionTypesCompatible,
    inferTypeArguments,
    lowerBackendProgram,
    lowerBackendProgramNative,
    renderBackendLLVMError,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM_, unless, void, when, zipWithM, zipWithM_)
import Control.Monad.State.Strict (StateT (StateT), evalStateT, get, gets, put, runStateT)
import Data.Bifunctor (first)
import Data.Char (ord)
import Data.List (find, intercalate, isPrefixOf, mapAccumL, nub, sort, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showHex)

import MLF.Backend.CallableShape
  ( BackendCallableBindingKind (..),
    BackendCallableHead (..),
    BackendCallableRef,
    backendCallableRef,
    backendCallableRefMatches,
    backendCallableRefName,
    backendCallableHead,
  )
import MLF.Backend.IR hiding
  ( BackendCallableBindingKind (..),
    BackendCallableHead (..),
    backendCallableHead,
  )
import MLF.Backend.IR.Types (closureEntryRefMatches)
import MLF.Backend.LLVM.Lower.Emit
import MLF.Backend.LLVM.Lower.Types
import MLF.Backend.LLVM.Syntax
import qualified MLF.Backend.StructuralRecursiveData as Structural
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), SymbolOwnerIdentity (..), symbolIdentityFromParts, symbolIdentityStableName, symbolRefMatches, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (constructorRefSymbol, deferredRefIdentity, envRefIdentity, IdDetails (..), IdentityGenerator, LocalRef, localRefIdentity, primitiveRefSymbol, idDetailsAliasMap, idDetailsSymbolIdentity, StructuralTypeBinderRole (..), TypeBinderIdentity, UniqueIdentity (..), freshIdentity, freshLocalRef, identityGeneratorAfter, initialIdentityGenerator, localIdentityStableUnique, typeBinderIdentityAliasMap, typeBinderIdentityAliasNames, typeBinderIdentityFromUnique, typeBinderIdentityStableName, typeBinderIdentityStructural)
import MLF.Util.Names (freshNameLike)

lowerBackendProgram :: BackendProgram -> Either BackendLLVMError LLVMModule
lowerBackendProgram program = do
  lowered <- lowerBackendProgramCore program
  let base = lpBase lowered
      needsIO = any functionReferencesIOWrapper (lpFunctions lowered)
      needsStringLength = any (functionReferencesGlobalNames (Set.singleton runtimeStringLengthName)) (lpFunctions lowered)
      needsStringIsEmpty = any (functionReferencesGlobalNames (Set.singleton runtimeStringIsEmptyName)) (lpFunctions lowered)
      needsStringContainsChar = any (functionReferencesGlobalNames (Set.singleton runtimeStringContainsCharName)) (lpFunctions lowered)
      needsStringContains = any (functionReferencesGlobalNames (Set.singleton runtimeStringContainsName)) (lpFunctions lowered)
      needsStringEquals = any (functionReferencesGlobalNames (Set.singleton runtimeStringEqualsName)) (lpFunctions lowered)
      needsStringStartsWith = any (functionReferencesGlobalNames (Set.singleton runtimeStringStartsWithName)) (lpFunctions lowered)
      needsStringEndsWith = any (functionReferencesGlobalNames (Set.singleton runtimeStringEndsWithName)) (lpFunctions lowered)
      needsStringAppend = any (functionReferencesGlobalNames (Set.singleton runtimeStringAppendName)) (lpFunctions lowered)
      needsStringReplaceChar = any (functionReferencesGlobalNames (Set.singleton runtimeStringReplaceCharName)) (lpFunctions lowered)
      needsStringReplace = any (functionReferencesGlobalNames (Set.singleton runtimeStringReplaceName)) (lpFunctions lowered)
      needsStringIndexOfChar = any (functionReferencesGlobalNames (Set.singleton runtimeStringIndexOfCharName)) (lpFunctions lowered)
      needsStringIndexOf = any (functionReferencesGlobalNames (Set.singleton runtimeStringIndexOfName)) (lpFunctions lowered)
      needsStringSplit = any (functionReferencesGlobalNames (Set.singleton runtimeStringSplitName)) (lpFunctions lowered)
      needsStringJoin = any (functionReferencesGlobalNames (Set.singleton runtimeStringJoinName)) (lpFunctions lowered)
      needsStringSplitChar = any (functionReferencesGlobalNames (Set.singleton runtimeStringSplitCharName)) (lpFunctions lowered)
      needsStringCompare = any (functionReferencesGlobalNames (Set.singleton runtimeStringCompareName)) (lpFunctions lowered)
      needsStringFromChar = any (functionReferencesGlobalNames (Set.singleton runtimeStringFromCharName)) (lpFunctions lowered)
      needsStringFromInt = any (functionReferencesGlobalNames (Set.singleton runtimeStringFromIntName)) (lpFunctions lowered)
      needsStringFromBool = any (functionReferencesGlobalNames (Set.singleton runtimeStringFromBoolName)) (lpFunctions lowered)
      needsStringFromNat = any (functionReferencesGlobalNames (Set.singleton runtimeStringFromNatName)) (lpFunctions lowered)
      needsStringFromList = any (functionReferencesGlobalNames (Set.singleton runtimeStringFromListName)) (lpFunctions lowered)
      needsStringToList = any (functionReferencesGlobalNames (Set.singleton runtimeStringToListName)) (lpFunctions lowered)
      needsStringDrop = any (functionReferencesGlobalNames (Set.singleton runtimeStringDropName)) (lpFunctions lowered)
      needsStringTake = any (functionReferencesGlobalNames (Set.singleton runtimeStringTakeName)) (lpFunctions lowered)
      needsStringSlice = any (functionReferencesGlobalNames (Set.singleton runtimeStringSliceName)) (lpFunctions lowered)
      needsStringCharAt = any (functionReferencesGlobalNames (Set.singleton runtimeStringCharAtName)) (lpFunctions lowered)
      needsStringCharAtOption = any (functionReferencesGlobalNames (Set.singleton runtimeStringCharAtOptionName)) (lpFunctions lowered)
      needsCharIsDigit = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsDigitName)) (lpFunctions lowered)
      needsCharIsAsciiLower = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiLowerName)) (lpFunctions lowered)
      needsCharIsAsciiUpper = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiUpperName)) (lpFunctions lowered)
      needsCharIsAsciiAlpha = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiAlphaName)) (lpFunctions lowered)
      needsCharIsAsciiAlphaNum = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiAlphaNumName)) (lpFunctions lowered)
      needsCharIsAsciiIdentifierStart = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiIdentifierStartName)) (lpFunctions lowered)
      needsCharIsAsciiIdentifierContinue = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiIdentifierContinueName)) (lpFunctions lowered)
      needsCharIsAsciiWhitespace = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiWhitespaceName)) (lpFunctions lowered)
      needsCharIsAsciiPunctuation = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiPunctuationName)) (lpFunctions lowered)
      needsCharIsAsciiPrintable = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiPrintableName)) (lpFunctions lowered)
      needsCharIsAsciiHexDigit = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiHexDigitName)) (lpFunctions lowered)
      needsCharIsAsciiLineBreak = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiLineBreakName)) (lpFunctions lowered)
      needsCharIsAsciiControl = any (functionReferencesGlobalNames (Set.singleton runtimeCharIsAsciiControlName)) (lpFunctions lowered)
      needsCharToAsciiLower = any (functionReferencesGlobalNames (Set.singleton runtimeCharToAsciiLowerName)) (lpFunctions lowered)
      needsCharToAsciiUpper = any (functionReferencesGlobalNames (Set.singleton runtimeCharToAsciiUpperName)) (lpFunctions lowered)
      needsStringToAsciiLower = any (functionReferencesGlobalNames (Set.singleton runtimeStringToAsciiLowerName)) (lpFunctions lowered)
      needsStringToAsciiUpper = any (functionReferencesGlobalNames (Set.singleton runtimeStringToAsciiUpperName)) (lpFunctions lowered)
      existingDecls =
        runtimeDeclarations
          base
          needsStringLength
          needsStringIsEmpty
          needsStringContainsChar
          needsStringContains
          needsStringEquals
          needsStringStartsWith
          needsStringEndsWith
          needsStringAppend
          needsStringReplaceChar
          needsStringReplace
          needsStringIndexOfChar
          needsStringIndexOf
          needsStringSplit
          needsStringJoin
          needsStringSplitChar
          needsStringCompare
          needsStringFromChar
          needsStringFromInt
          needsStringFromBool
          needsStringFromNat
          needsStringFromList
          needsStringToList
          needsStringDrop
          needsStringTake
          needsStringSlice
          needsStringCharAt
          needsStringCharAtOption
          needsCharIsDigit
          needsCharIsAsciiLower
          needsCharIsAsciiUpper
          needsCharIsAsciiAlpha
          needsCharIsAsciiAlphaNum
          needsCharIsAsciiIdentifierStart
          needsCharIsAsciiIdentifierContinue
          needsCharIsAsciiWhitespace
          needsCharIsAsciiPunctuation
          needsCharIsAsciiPrintable
          needsCharIsAsciiHexDigit
          needsCharIsAsciiLineBreak
          needsCharIsAsciiControl
          needsCharToAsciiLower
          needsCharToAsciiUpper
          needsStringToAsciiLower
          needsStringToAsciiUpper
      existingNames = Set.fromList (map llvmDeclarationName existingDecls)
      extraDecls
        | needsIO = filter (\d -> Set.notMember (llvmDeclarationName d) existingNames) (nativeRuntimeDeclarations base)
        | otherwise = []
  validateLLVMModuleSymbols
    LLVMModule
      { llvmModuleGlobals = rawLLVMGlobals lowered,
        llvmModuleDeclarations = existingDecls ++ extraDecls,
        llvmModuleFunctions = lpFunctions lowered ++ if needsIO then nativeIOFunctions base else []
      }

lowerBackendProgramNative :: BackendProgram -> Either BackendLLVMError LLVMModule
lowerBackendProgramNative program = do
  lowered <- lowerBackendProgramCore program
  lowerNativeProgram lowered

lowerBackendProgramCore :: BackendProgram -> Either BackendLLVMError LoweredProgram
lowerBackendProgramCore program0 = do
  first BackendLLVMValidationFailed (validateRawBackendBinderUniqueness program0)
  let program = assignBackendIdentitiesInProgram program0
  first BackendLLVMValidationFailed (validateBackendProgram program)
  base <- buildProgramBase program
  mainBinding <- requireProgramMainBinding base program
  reachable <- reachableBindings base mainBinding
  (generatorAfterSpecializations, specializations) <- collectRequiredSpecializations (pbIdentityGenerator base) base reachable
  let (generatorAfterEvidenceWrappers, evidenceWrappers) = collectEvidenceWrappers generatorAfterSpecializations base reachable specializations
      (generatorAfterFunctionWrappers, functionWrappers) = collectFunctionWrappers generatorAfterEvidenceWrappers base reachable specializations
      closureEntries0 = collectClosureEntries base reachable specializations evidenceWrappers functionWrappers
      referencedFunctions = collectReferencedFunctions base reachable specializations evidenceWrappers functionWrappers
      stringGlobals = assignStringGlobals (collectProgramStrings reachable specializations evidenceWrappers functionWrappers)
      env =
        ProgramEnv
          { peBase = base,
            peSpecializations = Map.fromList [(specializationKey (spRequest spec), spec) | spec <- specializations],
            peEvidenceWrappers = Map.fromList [(wrapperKey wrapper, wrapper) | wrapper <- evidenceWrappers],
            peFunctionWrappers = Map.fromList [(wrapperKey wrapper, wrapper) | wrapper <- functionWrappers],
            peStringGlobals = stringGlobals
          }
  uniqueClosureEntries <- requireUniqueClosureEntries closureEntries0
  let (generatorAfterClosureEntries, closureEntries) = assignGeneratedClosureEntryIdentities generatorAfterFunctionWrappers uniqueClosureEntries
  (functions, _) <-
    lowerFunctionJobs
      generatorAfterClosureEntries
      ( [ lowerMonomorphicBinding env binding
          | binding <- filter (shouldLowerReachableBinding referencedFunctions) reachable
        ]
          ++ [ lowerSpecialization env specialization
               | specialization <- filter (shouldLowerSpecialization referencedFunctions) specializations
             ]
          ++ [lowerEvidenceWrapper env wrapper | wrapper <- evidenceWrappers]
          ++ [lowerFunctionWrapper env wrapper | wrapper <- functionWrappers]
          ++ [lowerClosureEntry env entry | entry <- closureEntries]
      )
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

data RawTermBinderKey
  = RawTermBinderIdentity LowerLocalKey
  | RawTermBinderName String
  deriving (Eq, Ord)

validateRawBackendBinderUniqueness :: BackendProgram -> Either BackendValidationError ()
validateRawBackendBinderUniqueness program =
  mapM_ validateBinding (concatMap backendModuleBindings (backendProgramModules program))
  where
    validateBinding =
      validateExpr . backendBindingExpr

    validateExpr =
      \case
        BackendVarWithIdentity {} -> pure ()
        BackendLit {} -> pure ()
        BackendLamWithIdentity _ _ _ _ body -> validateExpr body
        BackendApp _ fun arg -> validateExpr fun >> validateExpr arg
        BackendLetWithIdentity _ _ _ _ rhs body -> validateExpr rhs >> validateExpr body
        BackendTyAbsWithIdentity _ _ _ _ body -> validateExpr body
        BackendTyApp _ fun _ -> validateExpr fun
        BackendConstructWithIdentity _ _ _ args -> mapM_ validateExpr args
        BackendCase _ scrutinee alternatives -> do
          validateExpr scrutinee
          mapM_ validateAlternative (NE.toList alternatives)
        BackendRoll _ payload -> validateExpr payload
        BackendUnroll _ payload -> validateExpr payload
        BackendClosureWithParamIdentities _ _ _ captures params body -> do
          requireUniqueRaw BackendDuplicateClosureCapture (map closureCaptureRef captures)
          requireUniqueRaw BackendDuplicateClosureParameter (map closureParamRef params)
          requireUniqueRaw BackendDuplicateClosureParameter (map closureCaptureRef captures ++ map closureParamRef params)
          mapM_ (validateExpr . backendClosureCaptureExpr) captures
          validateExpr body
        BackendClosureCall _ fun args -> validateExpr fun >> mapM_ validateExpr args

    validateAlternative (BackendAlternative pattern0 body) = do
      validatePattern pattern0
      validateExpr body

    validatePattern =
      \case
        BackendDefaultPattern -> pure ()
        BackendConstructorPatternWithBinderIdentities _ _ binders ->
          requireUniqueRaw BackendDuplicatePatternBinding (map patternBinderRefRaw binders)

    closureCaptureRef capture =
      rawTermBinderRef (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture)

    closureParamRef param =
      rawTermBinderRef (backendClosureParamIdentity param) (backendClosureParamName param)

    patternBinderRefRaw binder =
      rawTermBinderRef (backendPatternBinderIdentity binder) (backendPatternBinderName binder)

    rawTermBinderRef mbIdentity name =
      (maybe (RawTermBinderName name) RawTermBinderIdentity (mbIdentity >>= lowerLocalKey), name)

    requireUniqueRaw mkError =
      go Set.empty
      where
        go _ [] = pure ()
        go seen ((key, name) : rest)
          | Set.member key seen = Left (mkError name)
          | otherwise = go (Set.insert key seen) rest

type BackendTypeBinderEnv = Map String (Maybe TypeBinderIdentity)
type BackendDataEnv = Map String SymbolIdentity
type BackendConstructorEnv = Map String SymbolIdentity
type BackendGlobalEnv = Map String SymbolIdentity
type BackendTermEnv = Map String (Maybe IdDetails)

insertBackendSymbolIdentity :: String -> SymbolIdentity -> Map String SymbolIdentity -> Map String SymbolIdentity
insertBackendSymbolIdentity name identity =
  Map.insert name identity

backendSymbolIdentityEntries :: String -> SymbolIdentity -> [(String, SymbolIdentity)]
backendSymbolIdentityEntries name identity =
  [(name, identity)]

insertUniqueBackendTypeBinderIdentity :: String -> TypeBinderIdentity -> BackendTypeBinderEnv -> BackendTypeBinderEnv
insertUniqueBackendTypeBinderIdentity name identity env =
  foldl (\env0 alias -> Map.alter insert alias env0) env (typeBinderIdentityAliasNames name identity)
  where
    insert Nothing =
      Just (Just identity)
    insert (Just (Just existing))
      | existing == identity = Just (Just existing)
      | otherwise = Just Nothing
    insert (Just Nothing) =
      Just Nothing

shadowBackendTypeBinderIdentity :: String -> TypeBinderIdentity -> BackendTypeBinderEnv -> BackendTypeBinderEnv
shadowBackendTypeBinderIdentity name identity env =
  fmap Just (typeBinderIdentityAliasMap [(name, identity)]) `Map.union` env

insertUniqueBackendTermIdentity :: String -> IdDetails -> BackendTermEnv -> BackendTermEnv
insertUniqueBackendTermIdentity name identity env =
  Map.foldrWithKey (\alias details env0 -> Map.alter (insert details) alias env0) env (idDetailsAliasMap [(name, identity)])
  where
    insert details Nothing =
      Just (Just details)
    insert details (Just (Just existing))
      | existing == details = Just (Just existing)
      | otherwise = Just Nothing
    insert _ (Just Nothing) =
      Just Nothing

shadowBackendTermIdentity :: String -> IdDetails -> BackendTermEnv -> BackendTermEnv
shadowBackendTermIdentity name identity env =
  Map.foldrWithKey (\alias details env0 -> Map.insert alias (Just details) env0) env (idDetailsAliasMap [(name, identity)])

unionUniqueBackendTermEnv :: BackendTermEnv -> BackendTermEnv -> BackendTermEnv
unionUniqueBackendTermEnv =
  Map.unionWith merge
  where
    merge (Just left) (Just right)
      | left == right = Just left
      | otherwise = Nothing
    merge _ _ =
      Nothing

assignBackendIdentitiesInProgram :: BackendProgram -> BackendProgram
assignBackendIdentitiesInProgram program =
  program
    { backendProgramModulesWithIdentity = modules',
      backendProgramMainIdentity = backendProgramMainIdentity program <|> Map.lookup (backendProgramMain program) globalEnv
    }
  where
    generator0 =
      identityGeneratorAfter (generatedIdentitiesInBackendProgram program)
    (generator1, modulesWithDataIdentities, dataEnv) =
      assignGlobalDataIdentities generator0 (backendProgramModulesWithIdentity program)
    (generator2, modulesWithData) =
      mapAccumL (assignModuleData dataEnv) generator1 modulesWithDataIdentities
    (generator3, modulesWithBindingIdentities, globalEnv) =
      assignGlobalBindingIdentities generator2 modulesWithData
    (_, modules') =
      mapAccumL (assignModuleBindingBodies dataEnv constructorEnv globalEnv) generator3 modulesWithBindingIdentities
    constructorEnv =
      backendConstructorEnv modulesWithData

assignGlobalDataIdentities :: IdentityGenerator -> [BackendModule] -> (IdentityGenerator, [BackendModule], BackendDataEnv)
assignGlobalDataIdentities generator modules0 =
  (generator', reverse modules', dataEnv)
  where
    (generator', modules', dataEnv) =
      foldl assignModuleDataIdentities (generator, [], Map.empty) modules0

    assignModuleDataIdentities (generator0, modulesAcc, env0) backendModule =
      let moduleName = backendModuleName backendModule
          (generator1, dataDecls', env1) =
            foldl (assignDataIdentity moduleName) (generator0, [], env0) (backendModuleData backendModule)
       in ( generator1,
            backendModule {backendModuleDataWithIdentity = dataDecls'} : modulesAcc,
            env1
          )

    assignDataIdentity moduleName (generator0, dataAcc, env0) dataDecl =
      let name = backendDataName dataDecl
          (identity, generator1) =
            case backendDataIdentity dataDecl of
              Just existing -> (existing, generator0)
              Nothing ->
                let (unique, generatorNext) = freshIdentity generator0
                 in (generatedBackendDataIdentity moduleName name unique, generatorNext)
          dataDecl' = dataDecl {backendDataIdentity = Just identity}
       in (generator1, dataAcc ++ [dataDecl'], insertBackendSymbolIdentity name identity env0)

backendConstructorEnv :: [BackendModule] -> BackendConstructorEnv
backendConstructorEnv modules0 =
  Map.fromList
    [ entry
    | backendModule <- modules0,
      dataDecl <- backendModuleData backendModule,
      constructor <- backendDataConstructors dataDecl,
      Just identity <- [backendConstructorIdentity constructor],
      entry <- backendSymbolIdentityEntries (backendConstructorName constructor) identity
    ]

assignModuleData :: BackendDataEnv -> IdentityGenerator -> BackendModule -> (IdentityGenerator, BackendModule)
assignModuleData dataEnv generator backendModule =
  ( generator1,
    backendModule
      { backendModuleDataWithIdentity = dataDecls',
        backendModuleBindingsWithIdentity = backendModuleBindings backendModule
      }
  )
  where
    (generator1, dataDecls') =
      mapAccumL (assignDataDeclaration dataEnv (backendModuleName backendModule)) generator (backendModuleData backendModule)

assignGlobalBindingIdentities :: IdentityGenerator -> [BackendModule] -> (IdentityGenerator, [BackendModule], BackendGlobalEnv)
assignGlobalBindingIdentities generator modules0 =
  (generator', reverse modules', globalEnv)
  where
    (generator', modules', globalEnv) =
      foldl assignModuleBindings (generator, [], Map.empty) modules0

    assignModuleBindings (generator0, modulesAcc, env0) backendModule =
      let moduleName = backendModuleName backendModule
          (generator1, bindings', env1) =
            foldl (assignBindingIdentity moduleName) (generator0, [], env0) (backendModuleBindings backendModule)
       in ( generator1,
            backendModule {backendModuleBindingsWithIdentity = bindings'} : modulesAcc,
            env1
          )

    assignBindingIdentity moduleName (generator0, bindingsAcc, env0) binding =
      let name = backendBindingName binding
          (identity, generator1) =
            case backendBindingIdentity binding of
              Just existing -> (existing, generator0)
              Nothing ->
                let (unique, generatorNext) = freshIdentity generator0
                 in (generatedBackendBindingIdentity moduleName name unique, generatorNext)
          binding' = binding {backendBindingIdentity = Just identity}
       in (generator1, bindingsAcc ++ [binding'], insertBackendSymbolIdentity name identity env0)

generatedBackendBindingIdentity :: String -> String -> UniqueIdentity -> SymbolIdentity
generatedBackendBindingIdentity moduleName name identity =
  symbolIdentityFromParts identity SymbolValue moduleName name Nothing

generatedBackendDataIdentity :: String -> String -> UniqueIdentity -> SymbolIdentity
generatedBackendDataIdentity moduleName name identity =
  symbolIdentityFromParts identity SymbolType moduleName name Nothing

assignModuleBindingBodies :: BackendDataEnv -> BackendConstructorEnv -> BackendGlobalEnv -> IdentityGenerator -> BackendModule -> (IdentityGenerator, BackendModule)
assignModuleBindingBodies dataEnv constructorEnv globalEnv generator backendModule =
  ( generator',
    backendModule {backendModuleBindingsWithIdentity = bindings'}
  )
  where
    (generator', bindings') =
      mapAccumL (assignBindingIdentities dataEnv constructorEnv globalEnv) generator (backendModuleBindings backendModule)

assignDataDeclaration :: BackendDataEnv -> String -> IdentityGenerator -> BackendData -> (IdentityGenerator, BackendData)
assignDataDeclaration dataEnv moduleName generator dataDecl =
  ( generator2,
    dataDecl
      { backendDataParameterRefsWithIdentity = parameterRefs',
        backendDataConstructorsWithIdentity = constructors'
      }
  )
  where
    (generator1, parameterRefs', env) =
      foldl assignParameter (generator, [], Map.empty) (backendDataParameterRefs dataDecl)
    (generator2, constructors') =
      mapAccumL (assignConstructorTypeBinderIdentities dataEnv moduleName (backendDataIdentity dataDecl) env) generator1 (backendDataConstructors dataDecl)

    assignParameter (generator0, refs, env0) ref =
      let name = backendDataParameterRefName ref
          oldIdentity = backendDataParameterRefIdentity ref
          (identity, generatorNext) = freshTypeBinderIdentity oldIdentity generator0
          ref' = backendDataParameterRefFromIdentity identity name
       in (generatorNext, refs ++ [ref'], insertUniqueBackendTypeBinderIdentity name identity env0)

assignConstructorTypeBinderIdentities :: BackendDataEnv -> String -> Maybe SymbolIdentity -> BackendTypeBinderEnv -> IdentityGenerator -> BackendConstructor -> (IdentityGenerator, BackendConstructor)
assignConstructorTypeBinderIdentities dataEnv moduleName mbDataIdentity env generator constructor =
  ( generator3,
    constructor
      { backendConstructorIdentity = Just constructorIdentity,
        backendConstructorForallsWithIdentity = foralls',
        backendConstructorFieldsWithIdentity = fields',
        backendConstructorResultWithIdentity = result'
      }
  )
  where
    (constructorIdentity, generator0) =
      freshConstructorIdentity (backendConstructorIdentity constructor) moduleName mbDataIdentity (backendConstructorName constructor) generator
    (generator1, foralls', env') =
      assignTypeBinders dataEnv env generator0 (backendConstructorForalls constructor)
    (generator2, fields') =
      mapAccumL (assignTypeBinderIdentitiesInType dataEnv env') generator1 (backendConstructorFields constructor)
    (generator3, result') =
      assignTypeBinderIdentitiesInType dataEnv env' generator2 (backendConstructorResult constructor)

assignBindingIdentities :: BackendDataEnv -> BackendConstructorEnv -> BackendGlobalEnv -> IdentityGenerator -> BackendBinding -> (IdentityGenerator, BackendBinding)
assignBindingIdentities dataEnv constructorEnv globalEnv generator binding =
  ( generator2,
    binding
      { backendBindingTypeWithMetadata = bindingTy',
        backendBindingExprWithMetadata = expr'
      }
  )
  where
    (generator1, bindingTy') =
      assignTypeBinderIdentitiesInType dataEnv Map.empty generator (backendBindingType binding)
    (generator2, expr') =
      assignIdentitiesInExpr dataEnv constructorEnv globalEnv Map.empty Map.empty generator1 (backendBindingExpr binding)

assignTypeBinders :: BackendDataEnv -> BackendTypeBinderEnv -> IdentityGenerator -> [BackendTypeBinder] -> (IdentityGenerator, [BackendTypeBinder], BackendTypeBinderEnv)
assignTypeBinders dataEnv env generator =
  foldl assignOne (generator, [], env)
  where
    assignOne (generator0, binders, env0) binder =
      let name = backendTypeBinderName binder
          (generator1, bound') =
            assignMaybeTypeBinderIdentitiesInType dataEnv env0 generator0 (backendTypeBinderBound binder)
          (identity, generator2) =
            freshTypeBinderIdentity (backendTypeBinderIdentity binder) generator1
          binder' =
            BackendTypeBinderWithIdentity
              (Just identity)
              name
              bound'
       in (generator2, binders ++ [binder'], insertUniqueBackendTypeBinderIdentity name identity env0)

assignMaybeTypeBinderIdentitiesInType :: BackendDataEnv -> BackendTypeBinderEnv -> IdentityGenerator -> Maybe BackendType -> (IdentityGenerator, Maybe BackendType)
assignMaybeTypeBinderIdentitiesInType dataEnv env generator =
  \case
    Nothing -> (generator, Nothing)
    Just ty ->
      let (generator', ty') = assignTypeBinderIdentitiesInType dataEnv env generator ty
       in (generator', Just ty')

assignTypeBinderIdentitiesInType :: BackendDataEnv -> BackendTypeBinderEnv -> IdentityGenerator -> BackendType -> (IdentityGenerator, BackendType)
assignTypeBinderIdentitiesInType dataEnv env generator ty =
  case ty of
    BTVarWithIdentity identity name ->
      (generator, BTVarWithIdentity (typeRefIdentity env identity name) name)
    BTArrow dom cod ->
      let (generator1, dom') = assignTypeBinderIdentitiesInType dataEnv env generator dom
          (generator2, cod') = assignTypeBinderIdentitiesInType dataEnv env generator1 cod
       in (generator2, BTArrow dom' cod')
    BTBaseWithIdentity identity base@(BaseTy name) ->
      (generator, BTBaseWithIdentity (dataHeadIdentity dataEnv identity name) base)
    BTConWithIdentity identity base@(BaseTy name) args ->
      let (generator', args') = assignNonEmptyTypes dataEnv env generator args
       in (generator', BTConWithIdentity (dataHeadIdentity dataEnv identity name) base args')
    BTVarAppWithIdentity identity name args ->
      let (generator', args') = assignNonEmptyTypes dataEnv env generator args
       in (generator', BTVarAppWithIdentity (typeRefIdentity env identity name) name args')
    BTForallWithIdentity identity name mbBound body ->
      let (generator1, mbBound') = assignMaybeTypeBinderIdentitiesInType dataEnv env generator mbBound
          (binderIdentity, generator2) = freshTypeBinderIdentity identity generator1
          env' = shadowBackendTypeBinderIdentity name binderIdentity env
          (generator3, body') = assignTypeBinderIdentitiesInType dataEnv env' generator2 body
       in (generator3, BTForallWithIdentity (Just binderIdentity) name mbBound' body')
    BTMuWithIdentity identity name body ->
      let (binderIdentity, generator1) = freshTypeBinderIdentity identity generator
          env' = shadowBackendTypeBinderIdentity name binderIdentity env
          (generator2, body') = assignTypeBinderIdentitiesInType dataEnv env' generator1 body
       in (generator2, BTMuWithIdentity (Just binderIdentity) name body')
    BTBottom ->
      (generator, BTBottom)

assignNonEmptyTypes :: BackendDataEnv -> BackendTypeBinderEnv -> IdentityGenerator -> NonEmpty BackendType -> (IdentityGenerator, NonEmpty BackendType)
assignNonEmptyTypes dataEnv env generator (ty :| tys) =
  let (generator1, ty') = assignTypeBinderIdentitiesInType dataEnv env generator ty
      (generator2, tys') = mapAccumL (assignTypeBinderIdentitiesInType dataEnv env) generator1 tys
   in (generator2, ty' :| tys')

assignIdentitiesInExpr :: BackendDataEnv -> BackendConstructorEnv -> BackendGlobalEnv -> BackendTypeBinderEnv -> BackendTermEnv -> IdentityGenerator -> BackendExpr -> (IdentityGenerator, BackendExpr)
assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator expr =
  case expr of
    BackendVarWithIdentity resultTy mbIdentity name ->
      let (generator', resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
       in (generator', BackendVarWithIdentity resultTy' (termRefIdentity globalEnv termEnv mbIdentity name) name)
    BackendLit resultTy lit ->
      let (generator', resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
       in (generator', BackendLit resultTy' lit)
    BackendLamWithIdentity resultTy mbIdentity name paramTy body ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, paramTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator1 paramTy
          (identity, generator3) = freshTermIdentity mbIdentity name generator2
          (generator4, body') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv (shadowBackendTermIdentity name identity termEnv) generator3 body
       in (generator4, BackendLamWithIdentity resultTy' (Just identity) name paramTy' body')
    BackendApp resultTy fun arg ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, fun') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 fun
          (generator3, arg') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator2 arg
       in (generator3, BackendApp resultTy' fun' arg')
    BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs body ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, bindingTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator1 bindingTy
          (generator3, rhs') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator2 rhs
          (identity, generator4) = freshTermIdentity mbIdentity name generator3
          (generator5, body') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv (shadowBackendTermIdentity name identity termEnv) generator4 body
       in (generator5, BackendLetWithIdentity resultTy' (Just identity) name bindingTy' rhs' body')
    BackendTyAbsWithIdentity resultTy identity name mbBound body ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, mbBound') = assignMaybeTypeBinderIdentitiesInType dataEnv typeEnv generator1 mbBound
          (binderIdentity, generator3) =
            case resultTy' of
              BTForallWithIdentity (Just resultIdentity) _ _ _ ->
                (resultIdentity, generator2)
              _ -> freshTypeBinderIdentity identity generator2
          typeEnv' = shadowBackendTypeBinderIdentity name binderIdentity typeEnv
          (generator4, body') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv' termEnv generator3 body
       in (generator4, BackendTyAbsWithIdentity resultTy' (Just binderIdentity) name mbBound' body')
    BackendTyApp resultTy fun argTy ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, fun') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 fun
          (generator3, argTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator2 argTy
       in (generator3, BackendTyApp resultTy' fun' argTy')
    BackendRoll resultTy payload ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, payload') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 payload
       in (generator2, BackendRoll resultTy' payload')
    BackendUnroll resultTy payload ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, payload') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 payload
       in (generator2, BackendUnroll resultTy' payload')
    BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (entryIdentity', generator2) = freshClosureEntryIdentity entryIdentity generator1
          (generator3, captures', captureEnv) = assignClosureCaptureIdentities dataEnv constructorEnv globalEnv typeEnv termEnv generator2 captures
          (generator4, params', paramEnv) = assignClosureParamIdentities dataEnv typeEnv generator3 params
          bodyEnv = unionUniqueBackendTermEnv paramEnv captureEnv
          (generator5, body') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv bodyEnv generator4 body
       in (generator5, BackendClosureWithParamIdentities resultTy' (Just entryIdentity') entryName captures' params' body')
    BackendClosureCall resultTy fun args ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, fun') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 fun
          (generator3, args') = mapAccumL (assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv) generator2 args
       in (generator3, BackendClosureCall resultTy' fun' args')
    BackendConstructWithIdentity resultTy mbIdentity name args ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, args') = mapAccumL (assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv) generator1 args
       in (generator2, BackendConstructWithIdentity resultTy' (constructorRefIdentity constructorEnv mbIdentity name) name args')
    BackendCase resultTy scrutinee alternatives ->
      let (generator1, resultTy') = assignTypeBinderIdentitiesInType dataEnv typeEnv generator resultTy
          (generator2, scrutinee') = assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 scrutinee
          (generator3, alternatives') = assignNonEmptyAlternatives dataEnv constructorEnv globalEnv typeEnv termEnv generator2 alternatives
       in (generator3, BackendCase resultTy' scrutinee' alternatives')

assignClosureCaptureIdentities :: BackendDataEnv -> BackendConstructorEnv -> BackendGlobalEnv -> BackendTypeBinderEnv -> BackendTermEnv -> IdentityGenerator -> [BackendClosureCapture] -> (IdentityGenerator, [BackendClosureCapture], BackendTermEnv)
assignClosureCaptureIdentities dataEnv constructorEnv globalEnv typeEnv termEnv generator captures =
  foldl assignOne (generator, [], Map.empty) captures
  where
    assignOne (generator0, captures0, captureEnv) capture =
      let (generator1, captureTy') =
            assignTypeBinderIdentitiesInType dataEnv typeEnv generator0 (backendClosureCaptureType capture)
          (generator2, captureExpr') =
            assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv termEnv generator1 (backendClosureCaptureExpr capture)
          name = backendClosureCaptureName capture
          (identity, generator3) =
            freshTermIdentity (backendClosureCaptureIdentity capture) name generator2
          capture' =
            capture
              { backendClosureCaptureIdentity = Just identity,
                backendClosureCaptureType = captureTy',
                backendClosureCaptureExpr = captureExpr'
              }
       in (generator3, captures0 ++ [capture'], insertUniqueBackendTermIdentity name identity captureEnv)

assignClosureParamIdentities :: BackendDataEnv -> BackendTypeBinderEnv -> IdentityGenerator -> [BackendClosureParam] -> (IdentityGenerator, [BackendClosureParam], BackendTermEnv)
assignClosureParamIdentities dataEnv typeEnv generator params =
  foldl assignOne (generator, [], Map.empty) params
  where
    assignOne (generator0, params0, paramEnv) param =
      let (generator1, paramTy') =
            assignTypeBinderIdentitiesInType dataEnv typeEnv generator0 (backendClosureParamType param)
          name = backendClosureParamName param
          (identity, generator2) =
            freshTermIdentity (backendClosureParamIdentity param) name generator1
          param' =
            param
              { backendClosureParamIdentity = Just identity,
                backendClosureParamType = paramTy'
              }
       in (generator2, params0 ++ [param'], insertUniqueBackendTermIdentity name identity paramEnv)

assignNonEmptyAlternatives :: BackendDataEnv -> BackendConstructorEnv -> BackendGlobalEnv -> BackendTypeBinderEnv -> BackendTermEnv -> IdentityGenerator -> NonEmpty BackendAlternative -> (IdentityGenerator, NonEmpty BackendAlternative)
assignNonEmptyAlternatives dataEnv constructorEnv globalEnv typeEnv termEnv generator (alternative :| alternatives) =
  let (generator1, alternative') = assignAlternativeIdentities dataEnv constructorEnv globalEnv typeEnv termEnv generator alternative
      (generator2, alternatives') = mapAccumL (assignAlternativeIdentities dataEnv constructorEnv globalEnv typeEnv termEnv) generator1 alternatives
   in (generator2, alternative' :| alternatives')

assignAlternativeIdentities :: BackendDataEnv -> BackendConstructorEnv -> BackendGlobalEnv -> BackendTypeBinderEnv -> BackendTermEnv -> IdentityGenerator -> BackendAlternative -> (IdentityGenerator, BackendAlternative)
assignAlternativeIdentities dataEnv constructorEnv globalEnv typeEnv termEnv generator alternative =
  ( generator',
    alternative {backendAltPattern = pattern', backendAltBody = body'}
  )
  where
    (generator1, pattern', patternEnv) =
      assignPatternIdentities constructorEnv generator (backendAltPattern alternative)
    (generator', body') =
      assignIdentitiesInExpr dataEnv constructorEnv globalEnv typeEnv (Map.union patternEnv termEnv) generator1 (backendAltBody alternative)

assignPatternIdentities :: BackendConstructorEnv -> IdentityGenerator -> BackendPattern -> (IdentityGenerator, BackendPattern, BackendTermEnv)
assignPatternIdentities constructorEnv generator =
  \case
    BackendDefaultPattern ->
      (generator, BackendDefaultPattern, Map.empty)
    BackendConstructorPatternWithBinderIdentities identity name binders ->
      let (generator', binders', env) =
            foldl assignBinder (generator, [], Map.empty) binders
       in (generator', BackendConstructorPatternWithBinderIdentities (constructorRefIdentity constructorEnv identity name) name binders', env)
  where
    assignBinder (generator0, binders, env) binder =
      let name = backendPatternBinderName binder
          (identity, generator1) =
            freshTermIdentity (backendPatternBinderIdentity binder) name generator0
          binder' = binder {backendPatternBinderIdentity = Just identity}
       in (generator1, binders ++ [binder'], insertUniqueBackendTermIdentity name identity env)

freshTypeBinderIdentity :: Maybe TypeBinderIdentity -> IdentityGenerator -> (TypeBinderIdentity, IdentityGenerator)
freshTypeBinderIdentity identity generator =
  case identity of
    Just resolvedIdentity -> (resolvedIdentity, generator)
    Nothing ->
      let (unique, generator') = freshIdentity generator
       in (typeBinderIdentityFromUnique unique, generator')

freshTermIdentity :: Maybe IdDetails -> String -> IdentityGenerator -> (IdDetails, IdentityGenerator)
freshTermIdentity (Just identity) _ generator =
  (identity, generator)
freshTermIdentity Nothing name generator =
  let (localRef, generator') = freshLocalRef name generator
   in (LocalId localRef, generator')

freshClosureEntryIdentity :: Maybe UniqueIdentity -> IdentityGenerator -> (UniqueIdentity, IdentityGenerator)
freshClosureEntryIdentity (Just identity) generator =
  (identity, generator)
freshClosureEntryIdentity Nothing generator =
  freshIdentity generator

freshConstructorIdentity :: Maybe SymbolIdentity -> String -> Maybe SymbolIdentity -> String -> IdentityGenerator -> (SymbolIdentity, IdentityGenerator)
freshConstructorIdentity (Just identity) _ _ _ generator =
  (identity, generator)
freshConstructorIdentity Nothing moduleName mbDataIdentity name generator =
  let (unique, generator') = freshIdentity generator
   in (generatedBackendConstructorIdentity moduleName mbDataIdentity name unique, generator')

generatedBackendConstructorIdentity :: String -> Maybe SymbolIdentity -> String -> UniqueIdentity -> SymbolIdentity
generatedBackendConstructorIdentity moduleName mbDataIdentity name identity =
  symbolIdentityFromParts identity SymbolConstructor moduleName name (SymbolOwnerType <$> mbDataIdentity)

typeRefIdentity :: BackendTypeBinderEnv -> Maybe TypeBinderIdentity -> String -> Maybe TypeBinderIdentity
typeRefIdentity env identity name =
  identity <|> (Map.lookup name env >>= id)

dataHeadIdentity :: BackendDataEnv -> Maybe SymbolIdentity -> String -> Maybe SymbolIdentity
dataHeadIdentity env identity name =
  identity <|> Map.lookup name env

termRefIdentity :: BackendGlobalEnv -> BackendTermEnv -> Maybe IdDetails -> String -> Maybe IdDetails
termRefIdentity globalEnv env identity name =
  identity <|> (Map.lookup name env >>= id) <|> (TopLevelId <$> Map.lookup name globalEnv)

constructorRefIdentity :: BackendConstructorEnv -> Maybe SymbolIdentity -> String -> Maybe SymbolIdentity
constructorRefIdentity env identity name =
  identity <|> Map.lookup name env

lowerNativeProgram :: LoweredProgram -> Either BackendLLVMError LLVMModule
lowerNativeProgram lowered = do
  let base = lpBase lowered
      env = lpEnv lowered
      mainBinding = lpMainBinding lowered
      mainForm = biForm mainBinding
  -- Reject self-referencing main bindings (from opaque placeholder fallback)
  -- which would cause infinite recursion in the native executable.
  case ffBody mainForm of
    BackendVarWithIdentity _ mbIdentity name
      | bindingSelfReference mainBinding mbIdentity name ->
          Left (BackendLLVMUnsupportedExpression "native process main"
            ("opaque main binding `" ++ biName mainBinding
             ++ "` could not be elaborated; its body is a self-reference placeholder"))
    _ -> pure ()
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
            ++ nativeRuntimeFunctions env
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
    | runtimeBindingNameAvailable base runtimeMallocName
  ]
    ++ [LLVMDeclaration nativePrintfName (LLVMInt 32) [LLVMPtr] True]
    ++ [LLVMDeclaration nativeSprintfName (LLVMInt 32) [LLVMPtr, LLVMPtr] True]
    ++ [LLVMDeclaration nativePutcharName (LLVMInt 32) [LLVMInt 32] False]
    ++ [LLVMDeclaration nativeReadLineName LLVMPtr [] False]
    ++ [LLVMDeclaration nativeReadFileName LLVMPtr [LLVMPtr] False]
    ++ [LLVMDeclaration nativeWriteFileName (LLVMInt 32) [LLVMPtr, LLVMPtr] False]
    ++ [LLVMDeclaration nativeAppendFileName (LLVMInt 32) [LLVMPtr, LLVMPtr] False]
    ++ [LLVMDeclaration nativeExitName (LLVMInt 32) [LLVMInt 64] False]
    ++ [LLVMDeclaration nativeGetArgsName LLVMPtr [] False]
    ++ [LLVMDeclaration nativeFreeArgsName (LLVMInt 32) [LLVMPtr] False]

nativeRuntimeFunctions :: ProgramEnv -> [LLVMFunction]
nativeRuntimeFunctions env =
  [nativeAndFunction | runtimeNameAvailable runtimeAndName]
    ++ [nativeStringByteLengthFunction (peStringGlobals env)]
    ++ [nativeStringRegisterLengthFunction]
    ++ [nativeStringLengthFunction | runtimeNameAvailable runtimeStringLengthName]
    ++ [nativeStringIsEmptyFunction | runtimeNameAvailable runtimeStringIsEmptyName]
    ++ [nativeStringContainsCharFunction | runtimeNameAvailable runtimeStringContainsCharName]
    ++ [nativeStringContainsFunction | runtimeNameAvailable runtimeStringContainsName]
    ++ [nativeStringEqualsFunction (peStringGlobals env) | runtimeNameAvailable runtimeStringEqualsName]
    ++ [nativeStringStartsWithFunction | runtimeNameAvailable runtimeStringStartsWithName]
    ++ [nativeStringEndsWithFunction | runtimeNameAvailable runtimeStringEndsWithName]
    ++ [nativeStringAppendFunction | runtimeNameAvailable runtimeStringAppendName]
    ++ [nativeStringReplaceCharFunction | runtimeNameAvailable runtimeStringReplaceCharName]
    ++ [nativeStringReplaceFunction | runtimeNameAvailable runtimeStringReplaceName]
    ++ [nativeStringIndexOfCharFunction | runtimeNameAvailable runtimeStringIndexOfCharName]
    ++ [nativeStringIndexOfFunction | runtimeNameAvailable runtimeStringIndexOfName]
    ++ [nativeStringSplitFunction | runtimeNameAvailable runtimeStringSplitName]
    ++ [nativeStringJoinFunction | runtimeNameAvailable runtimeStringJoinName]
    ++ [nativeStringSplitCharFunction | runtimeNameAvailable runtimeStringSplitCharName]
    ++ [nativeStringCompareFunction | runtimeNameAvailable runtimeStringCompareName]
    ++ [nativeStringFromCharFunction | runtimeNameAvailable runtimeStringFromCharName]
    ++ [nativeStringFromIntFunction | runtimeNameAvailable runtimeStringFromIntName]
    ++ [nativeStringFromBoolFunction | runtimeNameAvailable runtimeStringFromBoolName]
    ++ [nativeStringFromNatFunction | runtimeNameAvailable runtimeStringFromNatName]
    ++ [nativeStringFromListFunction | runtimeNameAvailable runtimeStringFromListName]
    ++ [nativeStringToListFunction | runtimeNameAvailable runtimeStringToListName]
    ++ [nativeStringDropFunction | runtimeNameAvailable runtimeStringDropName]
    ++ [nativeStringTakeFunction | runtimeNameAvailable runtimeStringTakeName]
    ++ [nativeStringSliceFunction | runtimeNameAvailable runtimeStringSliceName]
    ++ [nativeStringCharAtFunction | runtimeNameAvailable runtimeStringCharAtName]
    ++ [nativeStringCharAtOptionFunction | runtimeNameAvailable runtimeStringCharAtOptionName]
    ++ [nativeCharIsDigitFunction | runtimeNameAvailable runtimeCharIsDigitName]
    ++ [nativeCharIsAsciiLowerFunction | runtimeNameAvailable runtimeCharIsAsciiLowerName]
    ++ [nativeCharIsAsciiUpperFunction | runtimeNameAvailable runtimeCharIsAsciiUpperName]
    ++ [nativeCharIsAsciiAlphaFunction | runtimeNameAvailable runtimeCharIsAsciiAlphaName]
    ++ [nativeCharIsAsciiAlphaNumFunction | runtimeNameAvailable runtimeCharIsAsciiAlphaNumName]
    ++ [nativeCharIsAsciiIdentifierStartFunction | runtimeNameAvailable runtimeCharIsAsciiIdentifierStartName]
    ++ [nativeCharIsAsciiIdentifierContinueFunction | runtimeNameAvailable runtimeCharIsAsciiIdentifierContinueName]
    ++ [nativeCharIsAsciiWhitespaceFunction | runtimeNameAvailable runtimeCharIsAsciiWhitespaceName]
    ++ [nativeCharIsAsciiPunctuationFunction | runtimeNameAvailable runtimeCharIsAsciiPunctuationName]
    ++ [nativeCharIsAsciiPrintableFunction | runtimeNameAvailable runtimeCharIsAsciiPrintableName]
    ++ [nativeCharIsAsciiHexDigitFunction | runtimeNameAvailable runtimeCharIsAsciiHexDigitName]
    ++ [nativeCharIsAsciiLineBreakFunction | runtimeNameAvailable runtimeCharIsAsciiLineBreakName]
    ++ [nativeCharIsAsciiControlFunction | runtimeNameAvailable runtimeCharIsAsciiControlName]
    ++ [nativeCharToAsciiLowerFunction | runtimeNameAvailable runtimeCharToAsciiLowerName]
    ++ [nativeCharToAsciiUpperFunction | runtimeNameAvailable runtimeCharToAsciiUpperName]
    ++ [nativeStringToAsciiLowerFunction | runtimeNameAvailable runtimeStringToAsciiLowerName]
    ++ [nativeStringToAsciiUpperFunction | runtimeNameAvailable runtimeStringToAsciiUpperName]
    ++ nativeIOFunctions base
  where
    base = peBase env
    bindingNames = programBindingRuntimeNames base
    runtimeNameAvailable name = Set.notMember name bindingNames

nativeCMainName :: String
nativeCMainName =
  "main"

nativePrintfName :: String
nativePrintfName =
  "printf"

nativeSprintfName :: String
nativeSprintfName =
  "sprintf"

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

nativeReadLineName :: String
nativeReadLineName =
  "mlfp_runtime_read_line"

nativeReadFileName :: String
nativeReadFileName =
  "mlfp_runtime_read_file"

nativeWriteFileName :: String
nativeWriteFileName =
  "mlfp_runtime_write_file"

nativeAppendFileName :: String
nativeAppendFileName =
  "mlfp_runtime_append_file"

nativeExitName :: String
nativeExitName =
  "mlfp_runtime_exit"

nativeGetArgsName :: String
nativeGetArgsName =
  "mlfp_runtime_get_args"

nativeFreeArgsName :: String
nativeFreeArgsName =
  "mlfp_runtime_free_args"

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

nativeStrFunctionName :: String
nativeStrFunctionName =
  "__mlfp_native_str_function"

nativeStringByteLengthFunctionName :: String
nativeStringByteLengthFunctionName =
  "__mlfp_native_string_byte_length"

nativeStringRegisterLengthFunctionName :: String
nativeStringRegisterLengthFunctionName =
  "__mlfp_native_register_string_length"

nativeStringLengthRegistryHeadName :: String
nativeStringLengthRegistryHeadName =
  "__mlfp_native_string_length_head"

nativePutcharName :: String
nativePutcharName =
  "putchar"

nativeGlobals :: ProgramBase -> [NativeRenderSpec] -> [LLVMGlobal]
nativeGlobals base renderSpecs =
  [ LLVMStringGlobal nativeFmtIntName "%ld",
    LLVMStringGlobal nativeFmtStringName "%s",
    LLVMStringGlobal nativeStrTrueName "true",
    LLVMStringGlobal nativeStrFalseName "false",
    LLVMStringGlobal nativeStrNewlineName "\n",
    LLVMStringGlobal nativeStrSpaceName " ",
    LLVMStringGlobal nativeStrOpenParenName "(",
    LLVMStringGlobal nativeStrCloseParenName ")",
    LLVMStringGlobal nativeStrFunctionName "<function>",
    LLVMVariableGlobal nativeStringLengthRegistryHeadName LLVMPtr LLVMNull
  ]
    ++ concatMap (constructorNameGlobals base) renderSpecs

constructorNameGlobals :: ProgramBase -> NativeRenderSpec -> [LLVMGlobal]
constructorNameGlobals base spec =
  case nativeDataRuntimeForType base (nrsType spec) of
    Nothing -> []
    Just dataRuntime0 ->
      [ LLVMStringGlobal (nativeConstructorGlobalName spec constructorRuntime) (displayConstructorName dataRuntime0 constructorRuntime)
      | constructorRuntime <- drConstructors dataRuntime0
      ]

nativeDataRuntimeForType :: ProgramBase -> BackendType -> Maybe DataRuntime
nativeDataRuntimeForType base =
  \case
    BTBaseWithIdentity identity (BaseTy name) -> lookupDataRuntimeByHead base identity name
    BTConWithIdentity identity (BaseTy name) _ -> lookupDataRuntimeByHead base identity name
    BTMuWithIdentity identity name _ -> lookupDataRuntimeForStructuralMu base identity name
    BTMu name _ -> lookupDataRuntimeForStructuralMu base Nothing name
    _ -> Nothing

lookupDataRuntimeByHead :: ProgramBase -> Maybe SymbolIdentity -> String -> Maybe DataRuntime
lookupDataRuntimeByHead base mbIdentity name =
  case mbIdentity of
    Just identity -> Map.lookup identity (pbDataByIdentity base)
    Nothing -> lookupDataRuntimeByName base name

lookupDataRuntimeForStructuralMu :: ProgramBase -> Maybe TypeBinderIdentity -> String -> Maybe DataRuntime
lookupDataRuntimeForStructuralMu base mbIdentity name =
  case structuralIdentityRuntime of
    Just runtime -> Just runtime
    Nothing
      | Just {} <- structuralSelfIdentityUnique mbIdentity -> Nothing
      | otherwise -> structuralNameRuntime
  where
    structuralIdentityRuntime =
      structuralSelfIdentityUnique mbIdentity >>= findDataRuntimeByUnique

    structuralNameRuntime =
      Structural.structuralRecursiveDataName name >>= lookupDataRuntimeByName base

    structuralSelfIdentityUnique identity = do
      selfIdentity <- identity
      (unique, StructuralSelfBinder) <- typeBinderIdentityStructural selfIdentity
      pure unique

    findDataRuntimeByUnique unique =
      case
        [ runtime
        | runtime <- Map.elems (pbDataByIdentity base),
          Just dataIdentity <- [backendDataIdentity (drData runtime)],
          symbolUniqueIdentity dataIdentity == unique
        ]
      of
        runtime : _ -> Just runtime
        [] -> Nothing

lookupDataRuntimeByName :: ProgramBase -> String -> Maybe DataRuntime
lookupDataRuntimeByName _ _ =
  Nothing

nativeConstructorGlobalName :: NativeRenderSpec -> ConstructorRuntime -> String
nativeConstructorGlobalName spec constructorRuntime =
  "__mlfp_native_ctor$" ++ backendTypeKey (nrsType spec) ++ "$" ++ show (crTag constructorRuntime)

nativeRendererName :: BackendType -> String
nativeRendererName ty =
  nativeRenderPrefix ++ backendTypeKey ty

rejectNativeSymbolConflicts :: ProgramBase -> [NativeRenderSpec] -> Either BackendLLVMError ()
rejectNativeSymbolConflicts base renderSpecs =
  case [name | name <- Set.toList (programBindingRuntimeNames base), nativeNameConflicts name] of
    name : _ ->
      Left (BackendLLVMUnsupportedExpression "native process" ("reserved native LLVM symbol " ++ show name))
    [] -> Right ()
  where
    reservedNames =
      Set.fromList
        ( [ nativeCMainName,
            nativePrintfName,
            nativeSprintfName,
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
            NativeString ->
              Right (NativeRenderSpec ty (nativeRendererName ty) : specs, Set.insert key seen)
            NativeIO ->
              Right (specs, Set.insert key seen)
            NativeFunction ->
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
  | NativeString
  | NativeData DataRuntime
  | NativeIO
  | NativeFunction
  | NativeUnsupported String

nativeRenderableKind :: ProgramBase -> BackendType -> NativeRenderableKind
nativeRenderableKind base ty =
  case ty of
    BTBaseWithIdentity identity baseTy@(BaseTy name)
      | backendBuiltinHeadMatches "Int" identity baseTy -> NativeScalar
      | backendBuiltinHeadMatches "Bool" identity baseTy -> NativeScalar
      | backendBuiltinHeadMatches "Char" identity baseTy -> NativeScalar
      | backendBuiltinHeadMatches "String" identity baseTy -> NativeString
      | backendBuiltinHeadMatches ioTypeName identity baseTy -> NativeIO
      | otherwise ->
          maybe (NativeUnsupported ("unknown native result type " ++ show name)) NativeData (lookupDataRuntimeByHead base identity name)
    BTConWithIdentity identity baseTy@(BaseTy name) _
      | backendBuiltinHeadMatches ioTypeName identity baseTy -> NativeIO
      | otherwise ->
          maybe (NativeUnsupported ("unknown native result type " ++ show name)) NativeData (lookupDataRuntimeByHead base identity name)
    BTBase (BaseTy name) ->
      maybe (NativeUnsupported ("unknown native result type " ++ show name)) NativeData (lookupDataRuntimeByHead base Nothing name)
    BTCon (BaseTy name) _ ->
      maybe (NativeUnsupported ("unknown native result type " ++ show name)) NativeData (lookupDataRuntimeByHead base Nothing name)
    BTArrow {} -> NativeFunction
    BTForall {} -> NativeUnsupported "polymorphic main values are not native-renderable"
    BTVar {} -> NativeUnsupported "type-variable main values are not native-renderable"
    BTVarApp {} -> NativeUnsupported "variable-headed main values are not native-renderable"
    BTMuWithIdentity identity name _ ->
      case lookupDataRuntimeForStructuralMu base identity name of
        Just dataRuntime0 -> NativeData dataRuntime0
        Nothing -> NativeUnsupported "structural recursive main values are not native-renderable"
    BTMu name _ ->
      case lookupDataRuntimeForStructuralMu base Nothing name of
        Just dataRuntime0 -> NativeData dataRuntime0
        Nothing -> NativeUnsupported "structural recursive main values are not native-renderable"
    BTBottom -> NativeUnsupported "bottom main values are not native-renderable"

ioTypeName :: String
ioTypeName =
  "IO"

backendBuiltinHeadMatches :: String -> Maybe SymbolIdentity -> BaseTy -> Bool
backendBuiltinHeadMatches builtinName (Just identity) _ =
  identity == PrimitiveInventory.builtinTypeIdentity builtinName
backendBuiltinHeadMatches _ Nothing _ =
  False

backendIntTy :: BackendType
backendIntTy =
  literalBackendType (LInt 0)

backendBoolTy :: BackendType
backendBoolTy =
  literalBackendType (LBool False)

backendStringTy :: BackendType
backendStringTy =
  literalBackendType (LString "")

backendCharTy :: BackendType
backendCharTy =
  literalBackendType (LChar '\0')

lowerNativeRenderer :: ProgramEnv -> Map String String -> NativeRenderSpec -> Either BackendLLVMError LLVMFunction
lowerNativeRenderer env renderMap spec =
  case nativeRenderableKind (peBase env) (nrsType spec) of
    NativeScalar ->
      lowerNativeScalarRenderer spec
    NativeString ->
      lowerNativeStringRenderer spec
    NativeIO ->
      Left (BackendLLVMUnsupportedExpression "native result rendering" "IO values are not renderable directly")
    NativeFunction ->
      lowerNativeFunctionRenderer spec
    NativeData dataRuntime0 ->
      lowerNativeDataRenderer env renderMap spec dataRuntime0
    NativeUnsupported detail ->
      Left (BackendLLVMUnsupportedExpression "native result rendering" detail)

lowerNativeScalarRenderer :: NativeRenderSpec -> Either BackendLLVMError LLVMFunction
lowerNativeScalarRenderer spec =
  case nrsType spec of
    BTBaseWithIdentity identity base | backendBuiltinHeadMatches "Int" identity base ->
      lowerNativeFunction
        (nrsFunctionName spec)
        (LLVMInt 32)
        [(LLVMInt 64, "value"), (LLVMInt 1, "parenthesize")]
        $ \params -> do
          let value = requireNativeParam "value" params
          _ <- emitPrintf nativeFmtIntName [(LLVMInt 64, value)]
          finishNativeSuccess
    BTBaseWithIdentity identity base | backendBuiltinHeadMatches "Bool" identity base ->
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
    BTBaseWithIdentity identity base | backendBuiltinHeadMatches "Char" identity base ->
      lowerNativeFunction
        (nrsFunctionName spec)
        (LLVMInt 32)
        [(LLVMInt 32, "value"), (LLVMInt 1, "parenthesize")]
        $ \params -> do
          let value = requireNativeParam "value" params
          printableAscii <- freshBlock "char.printable.ascii"
          numericEscape <- freshBlock "char.numeric.escape"
          rejectQuote <- freshBlock "char.reject.quote"
          rejectBackslash <- freshBlock "char.reject.backslash"
          aboveControl <- emitAssign "char.above.control" (LLVMInt 1) (LLVMICmpUgt value (LLVMIntLiteral 32 31))
          belowDelete <- emitAssign "char.below.delete" (LLVMInt 1) (LLVMICmpUgt (LLVMIntLiteral 32 127) value)
          isPrintable <- emitAssign "char.printable" (LLVMInt 1) (LLVMAnd aboveControl belowDelete)
          finishCurrentBlock (LLVMSwitch (LLVMInt 1) isPrintable numericEscape [(1, rejectQuote)])
          startBlock rejectQuote
          isQuote <- emitAssign "char.is.quote" (LLVMInt 1) (LLVMICmpEq value (LLVMIntLiteral 32 (toInteger (ord '\''))))
          finishCurrentBlock (LLVMSwitch (LLVMInt 1) isQuote rejectBackslash [(1, numericEscape)])
          startBlock rejectBackslash
          isBackslash <- emitAssign "char.is.backslash" (LLVMInt 1) (LLVMICmpEq value (LLVMIntLiteral 32 (toInteger (ord '\\'))))
          finishCurrentBlock (LLVMSwitch (LLVMInt 1) isBackslash printableAscii [(1, numericEscape)])
          startBlock printableAscii
          _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\'')))
          _ <- emitPutchar value
          _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\'')))
          finishNativeSuccess
          startBlock numericEscape
          valueI64 <- emitAssign "char.code.i64" (LLVMInt 64) (LLVMZext value (LLVMInt 64))
          _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\'')))
          _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
          _ <- emitPrintf nativeFmtIntName [(LLVMInt 64, valueI64)]
          _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\'')))
          finishNativeSuccess
    _ ->
      Left (BackendLLVMUnsupportedExpression "native result rendering" ("unsupported scalar renderer " ++ show (nrsType spec)))

lowerNativeStringRenderer :: NativeRenderSpec -> Either BackendLLVMError LLVMFunction
lowerNativeStringRenderer spec =
  lowerNativeFunction
    (nrsFunctionName spec)
    (LLVMInt 32)
    [(LLVMPtr, "value"), (LLVMInt 1, "parenthesize")]
    $ \params -> do
      let value = requireNativeParam "value" params
      let i8Ty = LLVMInt 8
      let i32Ty = LLVMInt 32
      let i64Ty = LLVMInt 64
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '"')))
      -- Allocate a stack slot for the current pointer
      curSlot <- emitAssign "str.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      offsetSlot <- emitAssign "str.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      byteLength <-
        emitAssign
          "str.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      emitStore LLVMPtr value curSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) offsetSlot
      loopHeader <- freshBlock "str.header"
      chkBsl <- freshBlock "str.chk.bsl"
      escBsl <- freshBlock "str.esc.bsl"
      chkQuo <- freshBlock "str.chk.quo"
      escQuo <- freshBlock "str.esc.quo"
      chkNl <- freshBlock "str.chk.nl"
      escNl <- freshBlock "str.esc.nl"
      chkCr <- freshBlock "str.chk.cr"
      escCr <- freshBlock "str.esc.cr"
      chkTab <- freshBlock "str.chk.tab"
      escTab <- freshBlock "str.esc.tab"
      chkPrint <- freshBlock "str.chk.print"
      chkAscii <- freshBlock "str.chk.ascii"
      printNormal <- freshBlock "str.normal"
      chkUtf8 <- freshBlock "str.chk.utf8"
      printUtf8Two <- freshBlock "str.utf8.two"
      printNp <- freshBlock "str.np"
      printNul <- freshBlock "str.nul"
      printQuestion <- freshBlock "str.question"
      loopNext <- freshBlock "str.next"
      loopNextTwo <- freshBlock "str.next.two"
      loopDone <- freshBlock "str.done"
      -- Entry
      finishCurrentBlock (LLVMBr loopHeader)
      -- Loop header: load current pointer and stop at recorded byte length, not C-string null.
      startBlock loopHeader
      offset <- emitAssign "str.offset" i64Ty (LLVMLoad i64Ty offsetSlot)
      isDone <- emitAssign "str.end" (LLVMInt 1) (LLVMICmpEq offset byteLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isDone chkBsl [(1, loopDone)])
      startBlock chkBsl
      curPtr <- emitAssign "str.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      charPtr <- emitAssign "str.cptr" LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
      charVal <- emitAssign "str.c" i8Ty (LLVMLoad i8Ty charPtr)
      -- Check: backslash
      isBsl <- emitAssign "str.v.bsl" (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 (toInteger (ord '\\'))))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isBsl chkQuo [(1, escBsl)])
      startBlock escBsl
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      finishCurrentBlock (LLVMBr loopNext)
      -- Check: double-quote
      startBlock chkQuo
      isQuo <- emitAssign "str.v.quo" (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 (toInteger (ord '"'))))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isQuo chkNl [(1, escQuo)])
      startBlock escQuo
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '"')))
      finishCurrentBlock (LLVMBr loopNext)
      -- Check: newline
      startBlock chkNl
      isNl <- emitAssign "str.v.nl" (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 10))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isNl chkCr [(1, escNl)])
      startBlock escNl
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord 'n')))
      finishCurrentBlock (LLVMBr loopNext)
      -- Check: carriage return
      startBlock chkCr
      isCr <- emitAssign "str.v.cr" (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 13))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isCr chkTab [(1, escCr)])
      startBlock escCr
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord 'r')))
      finishCurrentBlock (LLVMBr loopNext)
      -- Check: tab
      startBlock chkTab
      isTab <- emitAssign "str.v.tab" (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 9))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTab chkPrint [(1, escTab)])
      startBlock escTab
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord 't')))
      finishCurrentBlock (LLVMBr loopNext)
      -- Check: printable (> 31)
      startBlock chkPrint
      isPrint <- emitAssign "str.v.pr" (LLVMInt 1) (LLVMICmpUgt charVal (LLVMIntLiteral 8 31))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isPrint printNp [(1, chkAscii)])
      -- ASCII bytes print directly; UTF-8 lead bytes print as escaped code points.
      startBlock chkAscii
      isAscii <- emitAssign "str.v.ascii" (LLVMInt 1) (LLVMICmpUgt (LLVMIntLiteral 8 128) charVal)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii chkUtf8 [(1, printNormal)])
      startBlock printNormal
      charZext <- emitAssign "str.zc" i32Ty (LLVMZext charVal i32Ty)
      _ <- emitPutchar charZext
      finishCurrentBlock (LLVMBr loopNext)
      startBlock chkUtf8
      utf8LeadClass <- emitAssign "str.u2.class" i8Ty (LLVMAnd charVal (LLVMIntLiteral 8 0xE0))
      isTwoByteUtf8 <- emitAssign "str.u2.is" (LLVMInt 1) (LLVMICmpEq utf8LeadClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwoByteUtf8 printNp [(1, printUtf8Two)])
      startBlock printUtf8Two
      contPtr <- emitAssign "str.u2.cptr" LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      contVal <- emitAssign "str.u2.c" i8Ty (LLVMLoad i8Ty contPtr)
      leadPayload <- emitAssign "str.u2.lead" i8Ty (LLVMAnd charVal (LLVMIntLiteral 8 0x1F))
      lead32 <- emitAssign "str.u2.lead32" i32Ty (LLVMZext leadPayload i32Ty)
      shifted <- emitAssign "str.u2.shift" i32Ty (LLVMShl lead32 (LLVMIntLiteral 32 6))
      contPayload <- emitAssign "str.u2.cont" i8Ty (LLVMAnd contVal (LLVMIntLiteral 8 0x3F))
      cont32 <- emitAssign "str.u2.cont32" i32Ty (LLVMZext contPayload i32Ty)
      code32 <- emitAssign "str.u2.code32" i32Ty (LLVMOr shifted cont32)
      code64 <- emitAssign "str.u2.code64" i64Ty (LLVMZext code32 i64Ty)
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPrintf nativeFmtIntName [(i64Ty, code64)]
      finishCurrentBlock (LLVMBr loopNextTwo)
      startBlock printNp
      isNul <- emitAssign "str.np.nul" (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isNul printQuestion [(1, printNul)])
      startBlock printNul
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '\\')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord 'N')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord 'U')))
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord 'L')))
      finishCurrentBlock (LLVMBr loopNext)
      startBlock printQuestion
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '?')))
      finishCurrentBlock (LLVMBr loopNext)
      -- Advance pointer
      startBlock loopNext
      nextPtr <- emitAssign "str.next" LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      nextOffset <- emitAssign "str.offset.next" i64Ty (LLVMAdd offset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr nextPtr curSlot
      emitStore i64Ty nextOffset offsetSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopNextTwo
      nextPtrTwo <- emitAssign "str.next.two" LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 2)])
      nextOffsetTwo <- emitAssign "str.offset.next.two" i64Ty (LLVMAdd offset (LLVMIntLiteral 64 2))
      emitStore LLVMPtr nextPtrTwo curSlot
      emitStore i64Ty nextOffsetTwo offsetSlot
      finishCurrentBlock (LLVMBr loopHeader)
      -- Done
      startBlock loopDone
      _ <- emitPutchar (LLVMIntLiteral 32 (toInteger (ord '"')))
      finishNativeSuccess

lowerNativeFunctionRenderer :: NativeRenderSpec -> Either BackendLLVMError LLVMFunction
lowerNativeFunctionRenderer spec =
  lowerNativeFunction
    (nrsFunctionName spec)
    (LLVMInt 32)
    [(LLVMPtr, "value"), (LLVMInt 1, "parenthesize")]
    $ \_params -> do
      _ <- emitPrintStringGlobal nativeStrFunctionName
      finishNativeSuccess

lowerNativeDataRenderer :: ProgramEnv -> Map String String -> NativeRenderSpec -> DataRuntime -> Either BackendLLVMError LLVMFunction
lowerNativeDataRenderer env renderMap spec dataRuntime0 =
  lowerNativeFunction
    (nrsFunctionName spec)
    (LLVMInt 32)
    [(LLVMPtr, "value"), (LLVMInt 1, "parenthesize")]
    $ \params -> do
      let value = requireNativeParam "value" params
          parenthesize = requireNativeParam "parenthesize" params
      tagPtr <- emitGep "native.tag.ptr" value constructorTagOffset
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
      fieldPtr <- emitGep "native.field.ptr" value (constructorFieldOffset index0)
      fieldValue <- emitAssign "native.field" fieldLLVMType (LLVMLoad fieldLLVMType fieldPtr)
      callNativeRenderer renderMap fieldTy fieldLLVMType fieldValue (nativeFieldParenthesize (peBase env) fieldTy)

nativeFieldParenthesize :: ProgramBase -> BackendType -> Bool
nativeFieldParenthesize base ty =
  case nativeRenderableKind base ty of
    NativeData {} -> True
    _ -> False

lowerNativeEntrypoint :: ProgramEnv -> BindingInfo -> Map String String -> Either BackendLLVMError LLVMFunction
lowerNativeEntrypoint env mainBinding renderMap =
  case nativeRenderableKind (peBase env) (ffReturnType mainForm) of
    NativeIO ->
      lowerNativeFunction nativeCMainName (LLVMInt 32) [] $ \_ -> do
        mainValue <- emitAssign "native.main" LLVMPtr (LLVMCall (biName mainBinding) [])
        -- Execute the IO action closure: load code+env, call
        codePtrField <- emitGep "io.main.code.ptr" mainValue 0
        codePtr <- emitAssign "io.main.code" LLVMPtr (LLVMLoad LLVMPtr codePtrField)
        envPtrField <- emitGep "io.main.env.ptr" mainValue 8
        envPtr <- emitAssign "io.main.env" LLVMPtr (LLVMLoad LLVMPtr envPtrField)
        _ <- emitAssign "io.main.exec" LLVMPtr (LLVMCallOperand codePtr [(LLVMPtr, envPtr)])
        finishNativeSuccess
    _ -> do
      lowerNativeFunction nativeCMainName (LLVMInt 32) [] $ \_ -> do
        if not (null (ffParams mainForm))
          then do
            -- Parameterized main (e.g. Bool -> Bool): render as <function>
            _ <- emitPrintStringGlobal nativeStrFunctionName
            _ <- emitPrintStringGlobal nativeStrNewlineName
            finishNativeSuccess
          else do
            mainLLVMType <- lowerBackendTypeM env "native process main result" (ffReturnType mainForm)
            mainValue <- emitAssign "native.main" mainLLVMType (LLVMCall (biName mainBinding) [])
            callNativeRenderer renderMap (ffReturnType mainForm) mainLLVMType mainValue False
            _ <- emitPrintStringGlobal nativeStrNewlineName
            finishNativeSuccess
  where
    mainForm = biForm mainBinding

nativeAndFunction :: LLVMFunction
nativeAndFunction =
  case
    lowerNativeFunction runtimeAndName (LLVMInt 1) [(LLVMInt 1, "left"), (LLVMInt 1, "right")] $ \params -> do
      result <- emitAssign "and" (LLVMInt 1) (LLVMAnd (requireNativeParam "left" params) (requireNativeParam "right" params))
      finishCurrentBlock (LLVMRet (LLVMInt 1) result)
  of
    Right function -> function
    Left err -> error ("internal native __mlfp_and lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsDigitFunction :: LLVMFunction
nativeCharIsDigitFunction =
  case
    lowerNativeFunction runtimeCharIsDigitName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeZero <- emitAssign "charisdigit.above.before.zero" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 47))
      belowAfterNine <- emitAssign "charisdigit.below.after.nine" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 58) value)
      result <- emitAssign "charisdigit.result" i1Ty (LLVMAnd aboveBeforeZero belowAfterNine)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_digit lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiLowerFunction :: LLVMFunction
nativeCharIsAsciiLowerFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiLowerName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeLowerA <- emitAssign "charisasciilower.above.before.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 96))
      belowAfterLowerZ <- emitAssign "charisasciilower.below.after.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 123) value)
      result <- emitAssign "charisasciilower.result" i1Ty (LLVMAnd aboveBeforeLowerA belowAfterLowerZ)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_lower lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiUpperFunction :: LLVMFunction
nativeCharIsAsciiUpperFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiUpperName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeUpperA <- emitAssign "charisasciiupper.above.before.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 64))
      belowAfterUpperZ <- emitAssign "charisasciiupper.below.after.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 91) value)
      result <- emitAssign "charisasciiupper.result" i1Ty (LLVMAnd aboveBeforeUpperA belowAfterUpperZ)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_upper lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiAlphaFunction :: LLVMFunction
nativeCharIsAsciiAlphaFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiAlphaName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeLowerA <- emitAssign "charisasciialpha.above.before.lower.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 96))
      belowAfterLowerZ <- emitAssign "charisasciialpha.below.after.lower.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 123) value)
      lowerResult <- emitAssign "charisasciialpha.lower.result" i1Ty (LLVMAnd aboveBeforeLowerA belowAfterLowerZ)
      aboveBeforeUpperA <- emitAssign "charisasciialpha.above.before.upper.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 64))
      belowAfterUpperZ <- emitAssign "charisasciialpha.below.after.upper.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 91) value)
      upperResult <- emitAssign "charisasciialpha.upper.result" i1Ty (LLVMAnd aboveBeforeUpperA belowAfterUpperZ)
      result <- emitAssign "charisasciialpha.result" i1Ty (LLVMOr lowerResult upperResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_alpha lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiAlphaNumFunction :: LLVMFunction
nativeCharIsAsciiAlphaNumFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiAlphaNumName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeLowerA <- emitAssign "charisasciialphanum.above.before.lower.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 96))
      belowAfterLowerZ <- emitAssign "charisasciialphanum.below.after.lower.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 123) value)
      lowerResult <- emitAssign "charisasciialphanum.lower.result" i1Ty (LLVMAnd aboveBeforeLowerA belowAfterLowerZ)
      aboveBeforeUpperA <- emitAssign "charisasciialphanum.above.before.upper.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 64))
      belowAfterUpperZ <- emitAssign "charisasciialphanum.below.after.upper.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 91) value)
      upperResult <- emitAssign "charisasciialphanum.upper.result" i1Ty (LLVMAnd aboveBeforeUpperA belowAfterUpperZ)
      alphaResult <- emitAssign "charisasciialphanum.alpha.result" i1Ty (LLVMOr lowerResult upperResult)
      aboveBeforeZero <- emitAssign "charisasciialphanum.above.before.zero" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 47))
      belowAfterNine <- emitAssign "charisasciialphanum.below.after.nine" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 58) value)
      digitResult <- emitAssign "charisasciialphanum.digit.result" i1Ty (LLVMAnd aboveBeforeZero belowAfterNine)
      result <- emitAssign "charisasciialphanum.result" i1Ty (LLVMOr alphaResult digitResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_alpha_num lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiIdentifierStartFunction :: LLVMFunction
nativeCharIsAsciiIdentifierStartFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiIdentifierStartName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeLowerA <- emitAssign "charisasciiidentifierstart.above.before.lower.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 96))
      belowAfterLowerZ <- emitAssign "charisasciiidentifierstart.below.after.lower.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 123) value)
      lowerResult <- emitAssign "charisasciiidentifierstart.lower.result" i1Ty (LLVMAnd aboveBeforeLowerA belowAfterLowerZ)
      aboveBeforeUpperA <- emitAssign "charisasciiidentifierstart.above.before.upper.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 64))
      belowAfterUpperZ <- emitAssign "charisasciiidentifierstart.below.after.upper.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 91) value)
      upperResult <- emitAssign "charisasciiidentifierstart.upper.result" i1Ty (LLVMAnd aboveBeforeUpperA belowAfterUpperZ)
      alphaResult <- emitAssign "charisasciiidentifierstart.alpha.result" i1Ty (LLVMOr lowerResult upperResult)
      underscoreResult <- emitAssign "charisasciiidentifierstart.underscore.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 95))
      result <- emitAssign "charisasciiidentifierstart.result" i1Ty (LLVMOr alphaResult underscoreResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_identifier_start lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiIdentifierContinueFunction :: LLVMFunction
nativeCharIsAsciiIdentifierContinueFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiIdentifierContinueName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeLowerA <- emitAssign "charisasciiidentifiercontinue.above.before.lower.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 96))
      belowAfterLowerZ <- emitAssign "charisasciiidentifiercontinue.below.after.lower.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 123) value)
      lowerResult <- emitAssign "charisasciiidentifiercontinue.lower.result" i1Ty (LLVMAnd aboveBeforeLowerA belowAfterLowerZ)
      aboveBeforeUpperA <- emitAssign "charisasciiidentifiercontinue.above.before.upper.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 64))
      belowAfterUpperZ <- emitAssign "charisasciiidentifiercontinue.below.after.upper.z" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 91) value)
      upperResult <- emitAssign "charisasciiidentifiercontinue.upper.result" i1Ty (LLVMAnd aboveBeforeUpperA belowAfterUpperZ)
      alphaResult <- emitAssign "charisasciiidentifiercontinue.alpha.result" i1Ty (LLVMOr lowerResult upperResult)
      aboveBeforeZero <- emitAssign "charisasciiidentifiercontinue.above.before.zero" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 47))
      belowAfterNine <- emitAssign "charisasciiidentifiercontinue.below.after.nine" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 58) value)
      digitResult <- emitAssign "charisasciiidentifiercontinue.digit.result" i1Ty (LLVMAnd aboveBeforeZero belowAfterNine)
      alphaNumResult <- emitAssign "charisasciiidentifiercontinue.alphanum.result" i1Ty (LLVMOr alphaResult digitResult)
      underscoreResult <- emitAssign "charisasciiidentifiercontinue.underscore.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 95))
      nameCharResult <- emitAssign "charisasciiidentifiercontinue.namechar.result" i1Ty (LLVMOr alphaNumResult underscoreResult)
      apostropheResult <- emitAssign "charisasciiidentifiercontinue.apostrophe.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 39))
      result <- emitAssign "charisasciiidentifiercontinue.result" i1Ty (LLVMOr nameCharResult apostropheResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_identifier_continue lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiWhitespaceFunction :: LLVMFunction
nativeCharIsAsciiWhitespaceFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiWhitespaceName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      spaceResult <- emitAssign "charisasciiwhitespace.space.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 32))
      tabResult <- emitAssign "charisasciiwhitespace.tab.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 9))
      newlineResult <- emitAssign "charisasciiwhitespace.newline.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 10))
      carriageReturnResult <- emitAssign "charisasciiwhitespace.carriagereturn.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 13))
      formFeedResult <- emitAssign "charisasciiwhitespace.formfeed.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 12))
      verticalTabResult <- emitAssign "charisasciiwhitespace.verticaltab.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 11))
      spaceOrTabResult <- emitAssign "charisasciiwhitespace.spaceortab.result" i1Ty (LLVMOr spaceResult tabResult)
      newlineOrCarriageReturnResult <- emitAssign "charisasciiwhitespace.newlineorcarriagereturn.result" i1Ty (LLVMOr newlineResult carriageReturnResult)
      formFeedOrVerticalTabResult <- emitAssign "charisasciiwhitespace.formfeedorverticaltab.result" i1Ty (LLVMOr formFeedResult verticalTabResult)
      firstHalfResult <- emitAssign "charisasciiwhitespace.firsthalf.result" i1Ty (LLVMOr spaceOrTabResult newlineOrCarriageReturnResult)
      result <- emitAssign "charisasciiwhitespace.result" i1Ty (LLVMOr firstHalfResult formFeedOrVerticalTabResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_whitespace lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiPunctuationFunction :: LLVMFunction
nativeCharIsAsciiPunctuationFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiPunctuationName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeBang <- emitAssign "charisasciipunctuation.above.before.bang" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 32))
      belowAfterSlash <- emitAssign "charisasciipunctuation.below.after.slash" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 48) value)
      bangToSlashResult <- emitAssign "charisasciipunctuation.bangtoslash.result" i1Ty (LLVMAnd aboveBeforeBang belowAfterSlash)
      aboveBeforeColon <- emitAssign "charisasciipunctuation.above.before.colon" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 57))
      belowAfterAt <- emitAssign "charisasciipunctuation.below.after.at" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 65) value)
      colonToAtResult <- emitAssign "charisasciipunctuation.colontoat.result" i1Ty (LLVMAnd aboveBeforeColon belowAfterAt)
      aboveBeforeLeftBracket <- emitAssign "charisasciipunctuation.above.before.leftbracket" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 90))
      belowAfterBacktick <- emitAssign "charisasciipunctuation.below.after.backtick" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 97) value)
      leftBracketToBacktickResult <- emitAssign "charisasciipunctuation.leftbrackettobacktick.result" i1Ty (LLVMAnd aboveBeforeLeftBracket belowAfterBacktick)
      aboveBeforeLeftBrace <- emitAssign "charisasciipunctuation.above.before.leftbrace" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 122))
      belowAfterTilde <- emitAssign "charisasciipunctuation.below.after.tilde" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 127) value)
      leftBraceToTildeResult <- emitAssign "charisasciipunctuation.leftbracetotilde.result" i1Ty (LLVMAnd aboveBeforeLeftBrace belowAfterTilde)
      firstHalfResult <- emitAssign "charisasciipunctuation.firsthalf.result" i1Ty (LLVMOr bangToSlashResult colonToAtResult)
      secondHalfResult <- emitAssign "charisasciipunctuation.secondhalf.result" i1Ty (LLVMOr leftBracketToBacktickResult leftBraceToTildeResult)
      result <- emitAssign "charisasciipunctuation.result" i1Ty (LLVMOr firstHalfResult secondHalfResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_punctuation lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiPrintableFunction :: LLVMFunction
nativeCharIsAsciiPrintableFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiPrintableName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeSpace <- emitAssign "charisasciiprintable.above.before.space" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 31))
      belowAfterTilde <- emitAssign "charisasciiprintable.below.after.tilde" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 127) value)
      result <- emitAssign "charisasciiprintable.result" i1Ty (LLVMAnd aboveBeforeSpace belowAfterTilde)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_printable lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiHexDigitFunction :: LLVMFunction
nativeCharIsAsciiHexDigitFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiHexDigitName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      aboveBeforeZero <- emitAssign "charisasciihex.above.before.zero" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 47))
      belowAfterNine <- emitAssign "charisasciihex.below.after.nine" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 58) value)
      digitResult <- emitAssign "charisasciihex.digit.result" i1Ty (LLVMAnd aboveBeforeZero belowAfterNine)
      aboveBeforeLowerA <- emitAssign "charisasciihex.above.before.lower.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 96))
      belowAfterLowerF <- emitAssign "charisasciihex.below.after.lower.f" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 103) value)
      lowerResult <- emitAssign "charisasciihex.lower.result" i1Ty (LLVMAnd aboveBeforeLowerA belowAfterLowerF)
      aboveBeforeUpperA <- emitAssign "charisasciihex.above.before.upper.a" i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 64))
      belowAfterUpperF <- emitAssign "charisasciihex.below.after.upper.f" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 71) value)
      upperResult <- emitAssign "charisasciihex.upper.result" i1Ty (LLVMAnd aboveBeforeUpperA belowAfterUpperF)
      alphaResult <- emitAssign "charisasciihex.alpha.result" i1Ty (LLVMOr lowerResult upperResult)
      result <- emitAssign "charisasciihex.result" i1Ty (LLVMOr digitResult alphaResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_hex_digit lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiLineBreakFunction :: LLVMFunction
nativeCharIsAsciiLineBreakFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiLineBreakName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      newlineResult <- emitAssign "charisasciilinebreak.newline.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 10))
      carriageReturnResult <- emitAssign "charisasciilinebreak.carriagereturn.result" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 13))
      result <- emitAssign "charisasciilinebreak.result" i1Ty (LLVMOr newlineResult carriageReturnResult)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_line_break lowering failed: " ++ renderBackendLLVMError err)

nativeCharIsAsciiControlFunction :: LLVMFunction
nativeCharIsAsciiControlFunction =
  case
    lowerNativeFunction runtimeCharIsAsciiControlName (LLVMInt 1) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
      belowSpace <- emitAssign "charisasciicontrol.below.space" i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 32) value)
      isDelete <- emitAssign "charisasciicontrol.delete" i1Ty (LLVMICmpEq value (LLVMIntLiteral 32 127))
      result <- emitAssign "charisasciicontrol.result" i1Ty (LLVMOr belowSpace isDelete)
      finishCurrentBlock (LLVMRet i1Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __char_is_ascii_control lowering failed: " ++ renderBackendLLVMError err)

nativeCharToAsciiLowerFunction :: LLVMFunction
nativeCharToAsciiLowerFunction =
  nativeCharAsciiCaseFunction
    runtimeCharToAsciiLowerName
    "chartoasciilower"
    64
    91
    (\value -> LLVMAdd value (LLVMIntLiteral 32 32))

nativeCharToAsciiUpperFunction :: LLVMFunction
nativeCharToAsciiUpperFunction =
  nativeCharAsciiCaseFunction
    runtimeCharToAsciiUpperName
    "chartoasciiupper"
    96
    123
    (\value -> LLVMSub value (LLVMIntLiteral 32 32))

nativeCharAsciiCaseFunction :: String -> String -> Integer -> Integer -> (LLVMOperand -> LLVMExpression) -> LLVMFunction
nativeCharAsciiCaseFunction functionName label lowerBound upperBound transform =
  case
    lowerNativeFunction functionName (LLVMInt 32) [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
          i32Ty = LLVMInt 32
      aboveLower <- emitAssign (label ++ ".above.lower") i1Ty (LLVMICmpUgt value (LLVMIntLiteral 32 lowerBound))
      belowUpper <- emitAssign (label ++ ".below.upper") i1Ty (LLVMICmpUgt (LLVMIntLiteral 32 upperBound) value)
      shouldTransform <- emitAssign (label ++ ".should.transform") i1Ty (LLVMAnd aboveLower belowUpper)
      transformBlock <- freshBlock (label ++ ".transform")
      unchangedBlock <- freshBlock (label ++ ".unchanged")
      finishCurrentBlock (LLVMSwitch i1Ty shouldTransform unchangedBlock [(1, transformBlock)])
      startBlock transformBlock
      transformed <- emitAssign (label ++ ".transformed") i32Ty (transform value)
      finishCurrentBlock (LLVMRet i32Ty transformed)
      startBlock unchangedBlock
      finishCurrentBlock (LLVMRet i32Ty value)
  of
    Right function -> function
    Left err -> error ("internal native ASCII char case lowering failed for " ++ functionName ++ ": " ++ renderBackendLLVMError err)

nativeStringLengthFunction :: LLVMFunction
nativeStringLengthFunction =
  case
    lowerNativeFunction runtimeStringLengthName (LLVMInt 64) [(LLVMPtr, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
      countSlot <- emitAssign "strlen.count.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      indexSlot <- emitAssign "strlen.index.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore i64Ty (LLVMIntLiteral 64 0) countSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) indexSlot
      byteLength <-
        emitAssign
          "strlen.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      loopHeader <- freshBlock "strlen.header"
      checkContinuation <- freshBlock "strlen.chk.continuation"
      incrementCount <- freshBlock "strlen.increment"
      loopNext <- freshBlock "strlen.next"
      loopDone <- freshBlock "strlen.done"
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      index <- emitAssign "strlen.index" i64Ty (LLVMLoad i64Ty indexSlot)
      complete <- emitAssign "strlen.complete" (LLVMInt 1) (LLVMICmpEq index byteLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) complete checkContinuation [(1, loopDone)])
      startBlock checkContinuation
      charPtr <- emitAssign "strlen.cptr" LLVMPtr (LLVMGetElementPtr i8Ty value [(i64Ty, index)])
      charVal <- emitAssign "strlen.c" i8Ty (LLVMLoad i8Ty charPtr)
      utf8Class <- emitAssign "strlen.utf8.class" i8Ty (LLVMAnd charVal (LLVMIntLiteral 8 0xC0))
      isContinuation <- emitAssign "strlen.is.continuation" (LLVMInt 1) (LLVMICmpEq utf8Class (LLVMIntLiteral 8 0x80))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isContinuation incrementCount [(1, loopNext)])
      startBlock incrementCount
      count <- emitAssign "strlen.count" i64Ty (LLVMLoad i64Ty countSlot)
      nextCount <- emitAssign "strlen.count.next" i64Ty (LLVMAdd count (LLVMIntLiteral 64 1))
      emitStore i64Ty nextCount countSlot
      finishCurrentBlock (LLVMBr loopNext)
      startBlock loopNext
      nextIndex <- emitAssign "strlen.index.next" i64Ty (LLVMAdd index (LLVMIntLiteral 64 1))
      emitStore i64Ty nextIndex indexSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopDone
      result <- emitAssign "strlen.result" i64Ty (LLVMLoad i64Ty countSlot)
      finishCurrentBlock (LLVMRet i64Ty result)
  of
    Right function -> function
    Left err -> error ("internal native __string_length lowering failed: " ++ renderBackendLLVMError err)

nativeStringIsEmptyFunction :: LLVMFunction
nativeStringIsEmptyFunction =
  case
    lowerNativeFunction runtimeStringIsEmptyName (LLVMInt 1) [(LLVMPtr, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i64Ty = LLVMInt 64
      byteLength <-
        emitAssign
          "strisempty.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      result <- emitAssign "strisempty.result" (LLVMInt 1) (LLVMICmpEq byteLength (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMRet (LLVMInt 1) result)
  of
    Right function -> function
    Left err -> error ("internal native __string_is_empty lowering failed: " ++ renderBackendLLVMError err)

nativeStringContainsCharFunction :: LLVMFunction
nativeStringContainsCharFunction =
  case
    lowerNativeFunction runtimeStringContainsCharName (LLVMInt 1) [(LLVMPtr, "value"), (LLVMInt 32, "needle")] $ \params -> do
      let value = requireNativeParam "value" params
          needle = requireNativeParam "needle" params
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          loadByte prefix curPtr offset = do
            bytePtr <- emitAssign (prefix ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
            emitAssign prefix i8Ty (LLVMLoad i8Ty bytePtr)
          extendByte prefix byte =
            emitAssign prefix i32Ty (LLVMZext byte i32Ty)
      curSlot <- emitAssign "strcontains.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr value curSlot
      scalarSlot <- emitAssign "strcontains.scalar.slot" LLVMPtr (LLVMAlloca i32Ty (LLVMIntLiteral 64 1))
      byteOffsetSlot <- emitAssign "strcontains.byte.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      byteLength <-
        emitAssign
          "strcontains.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      emitStore i64Ty (LLVMIntLiteral 64 0) byteOffsetSlot
      loopHeader <- freshBlock "strcontains.header"
      detectStart <- freshBlock "strcontains.detect.start"
      detectAscii <- freshBlock "strcontains.detect.ascii"
      asciiScalar <- freshBlock "strcontains.ascii"
      detectTwo <- freshBlock "strcontains.detect.two"
      twoByteScalar <- freshBlock "strcontains.two"
      detectThree <- freshBlock "strcontains.detect.three"
      threeByteScalar <- freshBlock "strcontains.three"
      fourByteScalar <- freshBlock "strcontains.four"
      compareScalar <- freshBlock "strcontains.compare"
      loopNext <- freshBlock "strcontains.next"
      found <- freshBlock "strcontains.found"
      notFound <- freshBlock "strcontains.not-found"
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      byteOffset <- emitAssign "strcontains.byte.offset" i64Ty (LLVMLoad i64Ty byteOffsetSlot)
      complete <- emitAssign "strcontains.complete" (LLVMInt 1) (LLVMICmpEq byteOffset byteLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) complete detectStart [(1, notFound)])
      startBlock detectStart
      curPtr <- emitAssign "strcontains.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      byte0 <- loadByte "strcontains.b0" curPtr 0
      utf8Class <- emitAssign "strcontains.utf8.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xC0))
      isContinuation <- emitAssign "strcontains.is.continuation" (LLVMInt 1) (LLVMICmpEq utf8Class (LLVMIntLiteral 8 0x80))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isContinuation detectAscii [(1, loopNext)])
      startBlock detectAscii
      asciiClass <- emitAssign "strcontains.ascii.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strcontains.is.ascii" (LLVMInt 1) (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii detectTwo [(1, asciiScalar)])
      startBlock asciiScalar
      asciiValue <- extendByte "strcontains.ascii.value" byte0
      emitStore i32Ty asciiValue scalarSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock detectTwo
      twoClass <- emitAssign "strcontains.two.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strcontains.is.two" (LLVMInt 1) (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo detectThree [(1, twoByteScalar)])
      startBlock twoByteScalar
      twoByte0Masked <- emitAssign "strcontains.two.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x1F))
      twoByte0Value <- extendByte "strcontains.two.b0.value" twoByte0Masked
      twoByte0Shifted <- emitAssign "strcontains.two.b0.shifted" i32Ty (LLVMShl twoByte0Value (LLVMIntLiteral 32 6))
      twoByte1 <- loadByte "strcontains.two.b1" curPtr 1
      twoByte1Masked <- emitAssign "strcontains.two.b1.masked" i8Ty (LLVMAnd twoByte1 (LLVMIntLiteral 8 0x3F))
      twoByte1Value <- extendByte "strcontains.two.b1.value" twoByte1Masked
      twoScalar <- emitAssign "strcontains.two.scalar" i32Ty (LLVMOr twoByte0Shifted twoByte1Value)
      emitStore i32Ty twoScalar scalarSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock detectThree
      threeClass <- emitAssign "strcontains.three.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strcontains.is.three" (LLVMInt 1) (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree fourByteScalar [(1, threeByteScalar)])
      startBlock threeByteScalar
      threeByte0Masked <- emitAssign "strcontains.three.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x0F))
      threeByte0Value <- extendByte "strcontains.three.b0.value" threeByte0Masked
      threeByte0Shifted <- emitAssign "strcontains.three.b0.shifted" i32Ty (LLVMShl threeByte0Value (LLVMIntLiteral 32 12))
      threeByte1 <- loadByte "strcontains.three.b1" curPtr 1
      threeByte1Masked <- emitAssign "strcontains.three.b1.masked" i8Ty (LLVMAnd threeByte1 (LLVMIntLiteral 8 0x3F))
      threeByte1Value <- extendByte "strcontains.three.b1.value" threeByte1Masked
      threeByte1Shifted <- emitAssign "strcontains.three.b1.shifted" i32Ty (LLVMShl threeByte1Value (LLVMIntLiteral 32 6))
      threePrefix <- emitAssign "strcontains.three.prefix" i32Ty (LLVMOr threeByte0Shifted threeByte1Shifted)
      threeByte2 <- loadByte "strcontains.three.b2" curPtr 2
      threeByte2Masked <- emitAssign "strcontains.three.b2.masked" i8Ty (LLVMAnd threeByte2 (LLVMIntLiteral 8 0x3F))
      threeByte2Value <- extendByte "strcontains.three.b2.value" threeByte2Masked
      threeScalar <- emitAssign "strcontains.three.scalar" i32Ty (LLVMOr threePrefix threeByte2Value)
      emitStore i32Ty threeScalar scalarSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock fourByteScalar
      fourByte0Masked <- emitAssign "strcontains.four.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x07))
      fourByte0Value <- extendByte "strcontains.four.b0.value" fourByte0Masked
      fourByte0Shifted <- emitAssign "strcontains.four.b0.shifted" i32Ty (LLVMShl fourByte0Value (LLVMIntLiteral 32 18))
      fourByte1 <- loadByte "strcontains.four.b1" curPtr 1
      fourByte1Masked <- emitAssign "strcontains.four.b1.masked" i8Ty (LLVMAnd fourByte1 (LLVMIntLiteral 8 0x3F))
      fourByte1Value <- extendByte "strcontains.four.b1.value" fourByte1Masked
      fourByte1Shifted <- emitAssign "strcontains.four.b1.shifted" i32Ty (LLVMShl fourByte1Value (LLVMIntLiteral 32 12))
      fourPrefix <- emitAssign "strcontains.four.prefix" i32Ty (LLVMOr fourByte0Shifted fourByte1Shifted)
      fourByte2 <- loadByte "strcontains.four.b2" curPtr 2
      fourByte2Masked <- emitAssign "strcontains.four.b2.masked" i8Ty (LLVMAnd fourByte2 (LLVMIntLiteral 8 0x3F))
      fourByte2Value <- extendByte "strcontains.four.b2.value" fourByte2Masked
      fourByte2Shifted <- emitAssign "strcontains.four.b2.shifted" i32Ty (LLVMShl fourByte2Value (LLVMIntLiteral 32 6))
      fourPrefix' <- emitAssign "strcontains.four.prefix2" i32Ty (LLVMOr fourPrefix fourByte2Shifted)
      fourByte3 <- loadByte "strcontains.four.b3" curPtr 3
      fourByte3Masked <- emitAssign "strcontains.four.b3.masked" i8Ty (LLVMAnd fourByte3 (LLVMIntLiteral 8 0x3F))
      fourByte3Value <- extendByte "strcontains.four.b3.value" fourByte3Masked
      fourScalar <- emitAssign "strcontains.four.scalar" i32Ty (LLVMOr fourPrefix' fourByte3Value)
      emitStore i32Ty fourScalar scalarSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock compareScalar
      scalar <- emitAssign "strcontains.scalar" i32Ty (LLVMLoad i32Ty scalarSlot)
      isMatch <- emitAssign "strcontains.match" (LLVMInt 1) (LLVMICmpEq scalar needle)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isMatch loopNext [(1, found)])
      startBlock loopNext
      nextPtr <- emitAssign "strcontains.next.ptr" LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      nextByteOffset <- emitAssign "strcontains.byte.offset.next" i64Ty (LLVMAdd byteOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr nextPtr curSlot
      emitStore i64Ty nextByteOffset byteOffsetSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock found
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 1))
      startBlock notFound
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 0))
  of
    Right function -> function
    Left err -> error ("internal native __string_contains_char lowering failed: " ++ renderBackendLLVMError err)

nativeStringContainsFunction :: LLVMFunction
nativeStringContainsFunction =
  case
    lowerNativeFunction runtimeStringContainsName (LLVMInt 1) [(LLVMPtr, "haystack"), (LLVMPtr, "needle")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          needle = requireNativeParam "needle" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte prefix curPtr = do
            bytePtr <- emitAssign (prefix ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
            emitAssign prefix i8Ty (LLVMLoad i8Ty bytePtr)
          advancePtr prefix curPtr =
            emitAssign prefix LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      candidateSlot <- emitAssign "strcontainsstr.candidate.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr haystack candidateSlot
      matchHaystackSlot <- emitAssign "strcontainsstr.match.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchNeedleSlot <- emitAssign "strcontainsstr.match.needle.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      candidateOffsetSlot <- emitAssign "strcontainsstr.candidate.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      matchHaystackOffsetSlot <- emitAssign "strcontainsstr.match.haystack.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      matchNeedleOffsetSlot <- emitAssign "strcontainsstr.match.needle.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      haystackLength <-
        emitAssign
          "strcontainsstr.haystack.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, haystack)])
      needleLength <-
        emitAssign
          "strcontainsstr.needle.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, needle)])
      emitStore i64Ty (LLVMIntLiteral 64 0) candidateOffsetSlot
      candidateHeader <- freshBlock "strcontainsstr.candidate.header"
      candidateStart <- freshBlock "strcontainsstr.candidate.start"
      matchHeader <- freshBlock "strcontainsstr.match.header"
      matchHaystackEnd <- freshBlock "strcontainsstr.match.haystack-end"
      matchCompare <- freshBlock "strcontainsstr.match.compare"
      matchAdvance <- freshBlock "strcontainsstr.match.advance"
      candidateNext <- freshBlock "strcontainsstr.candidate.next"
      found <- freshBlock "strcontainsstr.found"
      notFound <- freshBlock "strcontainsstr.not-found"
      needleEmpty <- emitAssign "strcontainsstr.needle.empty" (LLVMInt 1) (LLVMICmpEq needleLength (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) needleEmpty candidateHeader [(1, found)])
      startBlock candidateHeader
      candidate <- emitAssign "strcontainsstr.candidate" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      candidateOffset <- emitAssign "strcontainsstr.candidate.offset" i64Ty (LLVMLoad i64Ty candidateOffsetSlot)
      candidateEnd <- emitAssign "strcontainsstr.candidate.end" (LLVMInt 1) (LLVMICmpEq candidateOffset haystackLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) candidateEnd candidateStart [(1, notFound)])
      startBlock candidateStart
      candidateByte <- loadByte "strcontainsstr.candidate.byte" candidate
      utf8Class <- emitAssign "strcontainsstr.utf8.class" i8Ty (LLVMAnd candidateByte (LLVMIntLiteral 8 0xC0))
      isContinuation <- emitAssign "strcontainsstr.is.continuation" (LLVMInt 1) (LLVMICmpEq utf8Class (LLVMIntLiteral 8 0x80))
      emitStore LLVMPtr candidate matchHaystackSlot
      emitStore LLVMPtr needle matchNeedleSlot
      emitStore i64Ty candidateOffset matchHaystackOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) matchNeedleOffsetSlot
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isContinuation matchHeader [(1, candidateNext)])
      startBlock matchHeader
      haystackCursor <- emitAssign "strcontainsstr.match.haystack" LLVMPtr (LLVMLoad LLVMPtr matchHaystackSlot)
      needleCursor <- emitAssign "strcontainsstr.match.needle" LLVMPtr (LLVMLoad LLVMPtr matchNeedleSlot)
      matchNeedleOffset <- emitAssign "strcontainsstr.match.needle.offset" i64Ty (LLVMLoad i64Ty matchNeedleOffsetSlot)
      needleDone <- emitAssign "strcontainsstr.match.needle.done" (LLVMInt 1) (LLVMICmpEq matchNeedleOffset needleLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) needleDone matchHaystackEnd [(1, found)])
      startBlock matchHaystackEnd
      matchHaystackOffset <- emitAssign "strcontainsstr.match.haystack.offset" i64Ty (LLVMLoad i64Ty matchHaystackOffsetSlot)
      haystackDone <- emitAssign "strcontainsstr.match.haystack.done" (LLVMInt 1) (LLVMICmpEq matchHaystackOffset haystackLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) haystackDone matchCompare [(1, candidateNext)])
      startBlock matchCompare
      needleByte <- loadByte "strcontainsstr.match.needle.byte" needleCursor
      haystackByte <- loadByte "strcontainsstr.match.haystack.byte" haystackCursor
      bytesMatch <- emitAssign "strcontainsstr.match.bytes" (LLVMInt 1) (LLVMICmpEq haystackByte needleByte)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) bytesMatch candidateNext [(1, matchAdvance)])
      startBlock matchAdvance
      nextHaystack <- advancePtr "strcontainsstr.match.haystack.next" haystackCursor
      nextNeedle <- advancePtr "strcontainsstr.match.needle.next" needleCursor
      nextMatchHaystackOffset <- emitAssign "strcontainsstr.match.haystack.offset.next" i64Ty (LLVMAdd matchHaystackOffset (LLVMIntLiteral 64 1))
      nextMatchNeedleOffset <- emitAssign "strcontainsstr.match.needle.offset.next" i64Ty (LLVMAdd matchNeedleOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr nextHaystack matchHaystackSlot
      emitStore LLVMPtr nextNeedle matchNeedleSlot
      emitStore i64Ty nextMatchHaystackOffset matchHaystackOffsetSlot
      emitStore i64Ty nextMatchNeedleOffset matchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock candidateNext
      nextCandidate <- advancePtr "strcontainsstr.candidate.next.ptr" candidate
      nextCandidateOffset <- emitAssign "strcontainsstr.candidate.offset.next" i64Ty (LLVMAdd candidateOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr nextCandidate candidateSlot
      emitStore i64Ty nextCandidateOffset candidateOffsetSlot
      finishCurrentBlock (LLVMBr candidateHeader)
      startBlock found
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 1))
      startBlock notFound
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 0))
  of
    Right function -> function
    Left err -> error ("internal native __string_contains lowering failed: " ++ renderBackendLLVMError err)

nativeStringByteLengthFunction :: Map String String -> LLVMFunction
nativeStringByteLengthFunction stringGlobals =
  case
    lowerNativeFunction nativeStringByteLengthFunctionName (LLVMInt 64) [(LLVMPtr, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          knownStrings =
            [ (globalName, toInteger (nativeStringByteLength stringValue))
            | (stringValue, globalName) <- Map.toAscList stringGlobals
            ]
          bytePtr label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
      registrySearch <- freshBlock "strbytes.registry"
      fallback <- freshBlock "strbytes.fallback"
      knownBlocks <-
        traverse
          ( \(globalName, byteLength) -> do
              check <- freshBlock "strbytes.known.check"
              matched <- freshBlock "strbytes.known.matched"
              pure (check, matched, globalName, byteLength)
          )
          knownStrings
      let firstBlock =
            case knownBlocks of
              (check, _, _, _) : _ -> check
              [] -> registrySearch
          nextBlocks =
            map (\(check, _, _, _) -> check) (drop 1 knownBlocks) ++ [registrySearch]
      finishCurrentBlock (LLVMBr firstBlock)
      zipWithM_
        ( \(check, matched, globalName, byteLength) nextBlock -> do
            startBlock check
            isKnown <- emitAssign "strbytes.known" i1Ty (LLVMICmpEq value (LLVMGlobalRef LLVMPtr globalName))
            finishCurrentBlock (LLVMSwitch i1Ty isKnown nextBlock [(1, matched)])
            startBlock matched
            finishCurrentBlock (LLVMRet i64Ty (LLVMIntLiteral 64 byteLength))
        )
        knownBlocks
        nextBlocks
      startBlock registrySearch
      entrySlot <- emitAssign "strbytes.registry.entry.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      headValue <- emitAssign "strbytes.registry.head" LLVMPtr (LLVMLoad LLVMPtr (LLVMGlobalRef LLVMPtr nativeStringLengthRegistryHeadName))
      emitStore LLVMPtr headValue entrySlot
      registryHeader <- freshBlock "strbytes.registry.header"
      registryCheck <- freshBlock "strbytes.registry.check"
      registryFound <- freshBlock "strbytes.registry.found"
      registryNext <- freshBlock "strbytes.registry.next"
      finishCurrentBlock (LLVMBr registryHeader)
      startBlock registryHeader
      entry <- emitAssign "strbytes.registry.entry" LLVMPtr (LLVMLoad LLVMPtr entrySlot)
      isRegistryEnd <- emitAssign "strbytes.registry.end" i1Ty (LLVMICmpEq entry LLVMNull)
      finishCurrentBlock (LLVMSwitch i1Ty isRegistryEnd registryCheck [(1, fallback)])
      startBlock registryCheck
      entryValuePtr <- bytePtr "strbytes.registry.value.ptr" entry 0
      entryValue <- emitAssign "strbytes.registry.value" LLVMPtr (LLVMLoad LLVMPtr entryValuePtr)
      registryMatched <- emitAssign "strbytes.registry.matched" i1Ty (LLVMICmpEq entryValue value)
      finishCurrentBlock (LLVMSwitch i1Ty registryMatched registryNext [(1, registryFound)])
      startBlock registryFound
      entryLengthPtr <- bytePtr "strbytes.registry.length.ptr" entry 8
      entryLength <- emitAssign "strbytes.registry.length" i64Ty (LLVMLoad i64Ty entryLengthPtr)
      finishCurrentBlock (LLVMRet i64Ty entryLength)
      startBlock registryNext
      entryNextPtr <- bytePtr "strbytes.registry.next.ptr" entry 16
      entryNext <- emitAssign "strbytes.registry.next" LLVMPtr (LLVMLoad LLVMPtr entryNextPtr)
      emitStore LLVMPtr entryNext entrySlot
      finishCurrentBlock (LLVMBr registryHeader)
      startBlock fallback
      cursorSlot <- emitAssign "strbytes.fallback.cursor.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      lengthSlot <- emitAssign "strbytes.fallback.length.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore LLVMPtr value cursorSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) lengthSlot
      loopHeader <- freshBlock "strbytes.fallback.header"
      advance <- freshBlock "strbytes.fallback.advance"
      fallbackDone <- freshBlock "strbytes.fallback.done"
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      cursor <- emitAssign "strbytes.fallback.cursor" LLVMPtr (LLVMLoad LLVMPtr cursorSlot)
      bytePtr0 <- bytePtr "strbytes.fallback.byte.ptr" cursor 0
      byte <- emitAssign "strbytes.fallback.byte" i8Ty (LLVMLoad i8Ty bytePtr0)
      isEnd <- emitAssign "strbytes.fallback.end" i1Ty (LLVMICmpEq byte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty isEnd advance [(1, fallbackDone)])
      startBlock advance
      currentLength <- emitAssign "strbytes.fallback.length" i64Ty (LLVMLoad i64Ty lengthSlot)
      nextLength <- emitAssign "strbytes.fallback.length.next" i64Ty (LLVMAdd currentLength (LLVMIntLiteral 64 1))
      nextCursor <- bytePtr "strbytes.fallback.cursor.next" cursor 1
      emitStore i64Ty nextLength lengthSlot
      emitStore LLVMPtr nextCursor cursorSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock fallbackDone
      fallbackLength <- emitAssign "strbytes.fallback.result" i64Ty (LLVMLoad i64Ty lengthSlot)
      finishCurrentBlock (LLVMRet i64Ty fallbackLength)
  of
    Right function -> function
    Left err -> error ("internal native string byte-length helper lowering failed: " ++ renderBackendLLVMError err)

nativeStringRegisterLengthFunction :: LLVMFunction
nativeStringRegisterLengthFunction =
  case
    lowerNativeFunction nativeStringRegisterLengthFunctionName (LLVMInt 32) [(LLVMPtr, "value"), (LLVMInt 64, "byte_length")] $ \params -> do
      let value = requireNativeParam "value" params
          byteLength = requireNativeParam "byte_length" params
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          i8Ty = LLVMInt 8
          bytePtr label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
      entry <- emitAssign "strregister.entry" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 24)])
      valuePtr <- bytePtr "strregister.value.ptr" entry 0
      lengthPtr <- bytePtr "strregister.length.ptr" entry 8
      nextPtr <- bytePtr "strregister.next.ptr" entry 16
      headValue <- emitAssign "strregister.head" LLVMPtr (LLVMLoad LLVMPtr (LLVMGlobalRef LLVMPtr nativeStringLengthRegistryHeadName))
      emitStore LLVMPtr value valuePtr
      emitStore i64Ty byteLength lengthPtr
      emitStore LLVMPtr headValue nextPtr
      emitStore LLVMPtr entry (LLVMGlobalRef LLVMPtr nativeStringLengthRegistryHeadName)
      finishCurrentBlock (LLVMRet i32Ty (LLVMIntLiteral 32 0))
  of
    Right function -> function
    Left err -> error ("internal native string length registry lowering failed: " ++ renderBackendLLVMError err)

nativeStringEqualsFunction :: Map String String -> LLVMFunction
nativeStringEqualsFunction _stringGlobals =
  case
    lowerNativeFunction runtimeStringEqualsName (LLVMInt 1) [(LLVMPtr, "left"), (LLVMPtr, "right")] $ \params -> do
      let left = requireNativeParam "left" params
          right = requireNativeParam "right" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte label curPtr = do
            bytePtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          advancePtr label curPtr =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      leftSlot <- emitAssign "strequals.left.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      rightSlot <- emitAssign "strequals.right.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      indexSlot <- emitAssign "strequals.index.slot" i64Ty (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore LLVMPtr left leftSlot
      emitStore LLVMPtr right rightSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) indexSlot
      leftLength <-
        emitAssign
          "strequals.left.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, left)])
      rightLength <-
        emitAssign
          "strequals.right.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, right)])
      lengthsMatch <- emitAssign "strequals.lengths.match" i1Ty (LLVMICmpEq leftLength rightLength)
      loopHeader <- freshBlock "strequals.header"
      advance <- freshBlock "strequals.advance"
      matched <- freshBlock "strequals.matched"
      notMatched <- freshBlock "strequals.not-matched"
      finishCurrentBlock (LLVMSwitch i1Ty lengthsMatch notMatched [(1, loopHeader)])
      startBlock loopHeader
      index <- emitAssign "strequals.index" i64Ty (LLVMLoad i64Ty indexSlot)
      complete <- emitAssign "strequals.complete" i1Ty (LLVMICmpEq index leftLength)
      compareBytes <- freshBlock "strequals.compare-bytes"
      finishCurrentBlock (LLVMSwitch i1Ty complete compareBytes [(1, matched)])
      startBlock compareBytes
      leftCursor <- emitAssign "strequals.left" LLVMPtr (LLVMLoad LLVMPtr leftSlot)
      rightCursor <- emitAssign "strequals.right" LLVMPtr (LLVMLoad LLVMPtr rightSlot)
      leftByte <- loadByte "strequals.left.byte" leftCursor
      rightByte <- loadByte "strequals.right.byte" rightCursor
      bytesMatch <- emitAssign "strequals.bytes.match" i1Ty (LLVMICmpEq leftByte rightByte)
      finishCurrentBlock (LLVMSwitch i1Ty bytesMatch notMatched [(1, advance)])
      startBlock advance
      nextIndex <- emitAssign "strequals.index.next" i64Ty (LLVMAdd index (LLVMIntLiteral 64 1))
      nextLeft <- advancePtr "strequals.left.next" leftCursor
      nextRight <- advancePtr "strequals.right.next" rightCursor
      emitStore i64Ty nextIndex indexSlot
      emitStore LLVMPtr nextLeft leftSlot
      emitStore LLVMPtr nextRight rightSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock matched
      finishCurrentBlock (LLVMRet i1Ty (LLVMIntLiteral 1 1))
      startBlock notMatched
      finishCurrentBlock (LLVMRet i1Ty (LLVMIntLiteral 1 0))
  of
    Right function -> function
    Left err -> error ("internal native __string_equals lowering failed: " ++ renderBackendLLVMError err)

nativeStringStartsWithFunction :: LLVMFunction
nativeStringStartsWithFunction =
  case
    lowerNativeFunction runtimeStringStartsWithName (LLVMInt 1) [(LLVMPtr, "haystack"), (LLVMPtr, "prefix")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          prefix = requireNativeParam "prefix" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte label curPtr = do
            bytePtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          advancePtr label curPtr =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      haystackSlot <- emitAssign "strstartswith.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      prefixSlot <- emitAssign "strstartswith.prefix.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr haystack haystackSlot
      emitStore LLVMPtr prefix prefixSlot
      matchHeader <- freshBlock "strstartswith.match.header"
      matchHaystackEnd <- freshBlock "strstartswith.match.haystack-end"
      matchCompare <- freshBlock "strstartswith.match.compare"
      matchAdvance <- freshBlock "strstartswith.match.advance"
      matched <- freshBlock "strstartswith.matched"
      notMatched <- freshBlock "strstartswith.not-matched"
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock matchHeader
      prefixCursor <- emitAssign "strstartswith.prefix" LLVMPtr (LLVMLoad LLVMPtr prefixSlot)
      prefixByte <- loadByte "strstartswith.prefix.byte" prefixCursor
      prefixDone <- emitAssign "strstartswith.prefix.done" (LLVMInt 1) (LLVMICmpEq prefixByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) prefixDone matchHaystackEnd [(1, matched)])
      startBlock matchHaystackEnd
      haystackCursor <- emitAssign "strstartswith.haystack" LLVMPtr (LLVMLoad LLVMPtr haystackSlot)
      haystackByte <- loadByte "strstartswith.haystack.byte" haystackCursor
      haystackDone <- emitAssign "strstartswith.haystack.done" (LLVMInt 1) (LLVMICmpEq haystackByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) haystackDone matchCompare [(1, notMatched)])
      startBlock matchCompare
      bytesMatch <- emitAssign "strstartswith.match.bytes" (LLVMInt 1) (LLVMICmpEq haystackByte prefixByte)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) bytesMatch notMatched [(1, matchAdvance)])
      startBlock matchAdvance
      nextHaystack <- advancePtr "strstartswith.haystack.next" haystackCursor
      nextPrefix <- advancePtr "strstartswith.prefix.next" prefixCursor
      emitStore LLVMPtr nextHaystack haystackSlot
      emitStore LLVMPtr nextPrefix prefixSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock matched
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 1))
      startBlock notMatched
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 0))
  of
    Right function -> function
    Left err -> error ("internal native __string_starts_with lowering failed: " ++ renderBackendLLVMError err)

nativeStringEndsWithFunction :: LLVMFunction
nativeStringEndsWithFunction =
  case
    lowerNativeFunction runtimeStringEndsWithName (LLVMInt 1) [(LLVMPtr, "haystack"), (LLVMPtr, "suffix")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          suffix = requireNativeParam "suffix" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte label curPtr = do
            bytePtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          advancePtr label curPtr =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      leadSlot <- emitAssign "strendswith.lead.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      lagSlot <- emitAssign "strendswith.lag.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      suffixAdvanceSlot <- emitAssign "strendswith.suffix.advance.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchHaystackSlot <- emitAssign "strendswith.match.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchSuffixSlot <- emitAssign "strendswith.match.suffix.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr haystack leadSlot
      emitStore LLVMPtr haystack lagSlot
      emitStore LLVMPtr suffix suffixAdvanceSlot
      leadAdvanceHeader <- freshBlock "strendswith.lead.advance.header"
      leadHasByte <- freshBlock "strendswith.lead.has-byte"
      leadAdvance <- freshBlock "strendswith.lead.advance"
      tailHeader <- freshBlock "strendswith.tail.header"
      tailAdvance <- freshBlock "strendswith.tail.advance"
      boundaryCheck <- freshBlock "strendswith.boundary.check"
      matchHeader <- freshBlock "strendswith.match.header"
      matchHaystackEnd <- freshBlock "strendswith.match.haystack-end"
      matchCompare <- freshBlock "strendswith.match.compare"
      matchAdvance <- freshBlock "strendswith.match.advance"
      matched <- freshBlock "strendswith.matched"
      notMatched <- freshBlock "strendswith.not-matched"
      firstSuffixByte <- loadByte "strendswith.suffix.first" suffix
      suffixEmpty <- emitAssign "strendswith.suffix.empty" (LLVMInt 1) (LLVMICmpEq firstSuffixByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) suffixEmpty leadAdvanceHeader [(1, matched)])
      startBlock leadAdvanceHeader
      suffixAdvanceCursor <- emitAssign "strendswith.suffix.advance" LLVMPtr (LLVMLoad LLVMPtr suffixAdvanceSlot)
      suffixAdvanceByte <- loadByte "strendswith.suffix.advance.byte" suffixAdvanceCursor
      suffixAdvanceDone <- emitAssign "strendswith.suffix.advance.done" (LLVMInt 1) (LLVMICmpEq suffixAdvanceByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) suffixAdvanceDone leadHasByte [(1, tailHeader)])
      startBlock leadHasByte
      leadCursor <- emitAssign "strendswith.lead" LLVMPtr (LLVMLoad LLVMPtr leadSlot)
      leadByte <- loadByte "strendswith.lead.byte" leadCursor
      leadDone <- emitAssign "strendswith.lead.done" (LLVMInt 1) (LLVMICmpEq leadByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) leadDone leadAdvance [(1, notMatched)])
      startBlock leadAdvance
      nextLead <- advancePtr "strendswith.lead.next" leadCursor
      nextSuffixAdvance <- advancePtr "strendswith.suffix.advance.next" suffixAdvanceCursor
      emitStore LLVMPtr nextLead leadSlot
      emitStore LLVMPtr nextSuffixAdvance suffixAdvanceSlot
      finishCurrentBlock (LLVMBr leadAdvanceHeader)
      startBlock tailHeader
      tailLeadCursor <- emitAssign "strendswith.tail.lead" LLVMPtr (LLVMLoad LLVMPtr leadSlot)
      tailLeadByte <- loadByte "strendswith.tail.lead.byte" tailLeadCursor
      tailLeadDone <- emitAssign "strendswith.tail.lead.done" (LLVMInt 1) (LLVMICmpEq tailLeadByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) tailLeadDone tailAdvance [(1, boundaryCheck)])
      startBlock tailAdvance
      nextTailLead <- advancePtr "strendswith.tail.lead.next" tailLeadCursor
      lagCursor <- emitAssign "strendswith.lag" LLVMPtr (LLVMLoad LLVMPtr lagSlot)
      nextLag <- advancePtr "strendswith.lag.next" lagCursor
      emitStore LLVMPtr nextTailLead leadSlot
      emitStore LLVMPtr nextLag lagSlot
      finishCurrentBlock (LLVMBr tailHeader)
      startBlock boundaryCheck
      suffixStart <- emitAssign "strendswith.suffix.start" LLVMPtr (LLVMLoad LLVMPtr lagSlot)
      suffixStartByte <- loadByte "strendswith.suffix.start.byte" suffixStart
      suffixStartClass <- emitAssign "strendswith.suffix.start.class" i8Ty (LLVMAnd suffixStartByte (LLVMIntLiteral 8 0xC0))
      suffixStartContinuation <- emitAssign "strendswith.suffix.start.continuation" (LLVMInt 1) (LLVMICmpEq suffixStartClass (LLVMIntLiteral 8 0x80))
      emitStore LLVMPtr suffixStart matchHaystackSlot
      emitStore LLVMPtr suffix matchSuffixSlot
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) suffixStartContinuation matchHeader [(1, notMatched)])
      startBlock matchHeader
      suffixCursor <- emitAssign "strendswith.match.suffix" LLVMPtr (LLVMLoad LLVMPtr matchSuffixSlot)
      suffixByte <- loadByte "strendswith.match.suffix.byte" suffixCursor
      suffixDone <- emitAssign "strendswith.match.suffix.done" (LLVMInt 1) (LLVMICmpEq suffixByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) suffixDone matchHaystackEnd [(1, matched)])
      startBlock matchHaystackEnd
      haystackCursor <- emitAssign "strendswith.match.haystack" LLVMPtr (LLVMLoad LLVMPtr matchHaystackSlot)
      haystackByte <- loadByte "strendswith.match.haystack.byte" haystackCursor
      haystackDone <- emitAssign "strendswith.match.haystack.done" (LLVMInt 1) (LLVMICmpEq haystackByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) haystackDone matchCompare [(1, notMatched)])
      startBlock matchCompare
      bytesMatch <- emitAssign "strendswith.match.bytes" (LLVMInt 1) (LLVMICmpEq haystackByte suffixByte)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) bytesMatch notMatched [(1, matchAdvance)])
      startBlock matchAdvance
      nextHaystack <- advancePtr "strendswith.match.haystack.next" haystackCursor
      nextSuffix <- advancePtr "strendswith.match.suffix.next" suffixCursor
      emitStore LLVMPtr nextHaystack matchHaystackSlot
      emitStore LLVMPtr nextSuffix matchSuffixSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock matched
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 1))
      startBlock notMatched
      finishCurrentBlock (LLVMRet (LLVMInt 1) (LLVMIntLiteral 1 0))
  of
    Right function -> function
    Left err -> error ("internal native __string_ends_with lowering failed: " ++ renderBackendLLVMError err)

nativeStringAppendFunction :: LLVMFunction
nativeStringAppendFunction =
  case
    lowerNativeFunction runtimeStringAppendName LLVMPtr [(LLVMPtr, "left"), (LLVMPtr, "right")] $ \params -> do
      let left = requireNativeParam "left" params
          right = requireNativeParam "right" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          ptrAt label curPtr offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
          loadByte label curPtr = do
            bytePtr <- ptrAt (label ++ ".ptr") curPtr 0
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          copyFixedBytes label sourceInitial destInitial totalCount = do
            sourceSlot <- emitAssign (label ++ ".source.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            destSlot <- emitAssign (label ++ ".dest.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            copiedSlot <- emitAssign (label ++ ".copied.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
            emitStore LLVMPtr sourceInitial sourceSlot
            emitStore LLVMPtr destInitial destSlot
            emitStore i64Ty (LLVMIntLiteral 64 0) copiedSlot
            header <- freshBlock (label ++ ".header")
            copy <- freshBlock (label ++ ".copy")
            done <- freshBlock (label ++ ".done")
            finishCurrentBlock (LLVMBr header)
            startBlock header
            copied <- emitAssign (label ++ ".copied") i64Ty (LLVMLoad i64Ty copiedSlot)
            copiedEnough <- emitAssign (label ++ ".copied.enough") (LLVMInt 1) (LLVMICmpEq copied totalCount)
            finishCurrentBlock (LLVMSwitch (LLVMInt 1) copiedEnough copy [(1, done)])
            startBlock copy
            source <- emitAssign (label ++ ".source") LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
            dest <- emitAssign (label ++ ".dest") LLVMPtr (LLVMLoad LLVMPtr destSlot)
            byte <- loadByte (label ++ ".byte") source
            destPtr <- ptrAt (label ++ ".dest.ptr") dest 0
            emitStore i8Ty byte destPtr
            nextSource <- ptrAt (label ++ ".source.next") source 1
            nextDest <- ptrAt (label ++ ".dest.next") dest 1
            nextCopied <- emitAssign (label ++ ".copied.next") i64Ty (LLVMAdd copied (LLVMIntLiteral 64 1))
            emitStore LLVMPtr nextSource sourceSlot
            emitStore LLVMPtr nextDest destSlot
            emitStore i64Ty nextCopied copiedSlot
            finishCurrentBlock (LLVMBr header)
            startBlock done
            emitAssign (label ++ ".dest.result") LLVMPtr (LLVMLoad LLVMPtr destSlot)
      leftBytes <-
        emitAssign
          "strappend.left.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, left)])
      rightBytes <-
        emitAssign
          "strappend.right.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, right)])
      combinedBytes <- emitAssign "strappend.combined.bytes" i64Ty (LLVMAdd leftBytes rightBytes)
      allocationSize <- emitAssign "strappend.allocation.size" i64Ty (LLVMAdd combinedBytes (LLVMIntLiteral 64 1))
      result <- emitAssign "strappend.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, allocationSize)])
      afterLeft <- copyFixedBytes "strappend.copy.left" left result leftBytes
      afterRight <- copyFixedBytes "strappend.copy.right" right afterLeft rightBytes
      terminatorPtr <- ptrAt "strappend.terminator.ptr" afterRight 0
      emitStore i8Ty (LLVMIntLiteral 8 0) terminatorPtr
      _ <-
        emitAssign
          "strappend.register.length"
          (LLVMInt 32)
          ( LLVMCall
              nativeStringRegisterLengthFunctionName
              [(LLVMPtr, result), (i64Ty, combinedBytes)]
          )
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_append lowering failed: " ++ renderBackendLLVMError err)

nativeStringReplaceCharFunction :: LLVMFunction
nativeStringReplaceCharFunction =
  case
    lowerNativeFunction runtimeStringReplaceCharName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 32, "needle"), (LLVMInt 32, "replacement")] $ \params -> do
      let value = requireNativeParam "value" params
          needle = requireNativeParam "needle" params
          replacement = requireNativeParam "replacement" params
          i32Ty = LLVMInt 32
      needleString <-
        emitAssign
          "strreplacechar.needle.string"
          LLVMPtr
          (LLVMCall runtimeStringFromCharName [(i32Ty, needle)])
      replacementString <-
        emitAssign
          "strreplacechar.replacement.string"
          LLVMPtr
          (LLVMCall runtimeStringFromCharName [(i32Ty, replacement)])
      result <-
        emitAssign
          "strreplacechar.result"
          LLVMPtr
          (LLVMCall runtimeStringReplaceName [(LLVMPtr, value), (LLVMPtr, needleString), (LLVMPtr, replacementString)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_replace_char lowering failed: " ++ renderBackendLLVMError err)

_nativeStringReplaceCharLegacyFunction :: LLVMFunction
_nativeStringReplaceCharLegacyFunction =
  case
    lowerNativeFunction runtimeStringReplaceCharName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 32, "needle"), (LLVMInt 32, "replacement")] $ \params -> do
      let value = requireNativeParam "value" params
          needle = requireNativeParam "needle" params
          replacement = requireNativeParam "replacement" params
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          loadByte prefix curPtr offset = do
            bytePtr <- emitAssign (prefix ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
            emitAssign prefix i8Ty (LLVMLoad i8Ty bytePtr)
          ptrAt prefix curPtr offset =
            emitAssign prefix LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
          ptrAtOperand prefix curPtr offset =
            emitAssign prefix LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, offset)])
          extendByte prefix byte =
            emitAssign prefix i32Ty (LLVMZext byte i32Ty)
          copySourceByte prefix source dest offset = do
            byte <- loadByte (prefix ++ ".byte") source offset
            destPtr <- ptrAt (prefix ++ ".dest") dest offset
            emitStore i8Ty byte destPtr
      scalarCount <-
        emitAssign
          "strreplace.scalar.count"
          i64Ty
          (LLVMCall runtimeStringLengthName [(LLVMPtr, value)])
      doubleScalarCount <- emitAssign "strreplace.scalar.count.double" i64Ty (LLVMAdd scalarCount scalarCount)
      maxBytes <- emitAssign "strreplace.max.bytes" i64Ty (LLVMAdd doubleScalarCount doubleScalarCount)
      allocationSize <- emitAssign "strreplace.allocation.size" i64Ty (LLVMAdd maxBytes (LLVMIntLiteral 64 1))
      result <- emitAssign "strreplace.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, allocationSize)])
      replacementString <-
        emitAssign
          "strreplace.replacement.string"
          LLVMPtr
          (LLVMCall runtimeStringFromCharName [(i32Ty, replacement)])
      sourceSlot <- emitAssign "strreplace.source.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      destSlot <- emitAssign "strreplace.dest.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      scalarSlot <- emitAssign "strreplace.scalar.slot" LLVMPtr (LLVMAlloca i32Ty (LLVMIntLiteral 64 1))
      byteLengthSlot <- emitAssign "strreplace.byte.length.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      replacementCursorSlot <- emitAssign "strreplace.replacement.cursor.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr value sourceSlot
      emitStore LLVMPtr result destSlot
      loopHeader <- freshBlock "strreplace.header"
      detectAscii <- freshBlock "strreplace.detect.ascii"
      asciiScalar <- freshBlock "strreplace.ascii"
      detectTwo <- freshBlock "strreplace.detect.two"
      twoByteScalar <- freshBlock "strreplace.two"
      detectThree <- freshBlock "strreplace.detect.three"
      threeByteScalar <- freshBlock "strreplace.three"
      fourByteScalar <- freshBlock "strreplace.four"
      compareScalar <- freshBlock "strreplace.compare"
      copyOriginal <- freshBlock "strreplace.copy.original"
      copyOriginalDetectTwo <- freshBlock "strreplace.copy.original.detect.two"
      copyOriginalDetectThree <- freshBlock "strreplace.copy.original.detect.three"
      copyOriginalOne <- freshBlock "strreplace.copy.original.one"
      copyOriginalTwo <- freshBlock "strreplace.copy.original.two"
      copyOriginalThree <- freshBlock "strreplace.copy.original.three"
      copyOriginalFour <- freshBlock "strreplace.copy.original.four"
      copyReplacement <- freshBlock "strreplace.copy.replacement"
      copyReplacementHeader <- freshBlock "strreplace.copy.replacement.header"
      copyReplacementBody <- freshBlock "strreplace.copy.replacement.body"
      copyReplacementDone <- freshBlock "strreplace.copy.replacement.done"
      done <- freshBlock "strreplace.done"
      let finishOriginalCopy prefix byteCount = do
            source' <- emitAssign (prefix ++ ".source") LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
            dest' <- emitAssign (prefix ++ ".dest") LLVMPtr (LLVMLoad LLVMPtr destSlot)
            mapM_ (copySourceByte prefix source' dest') [0 .. byteCount - 1]
            let byteCountOperand = LLVMIntLiteral 64 (toInteger byteCount)
            nextSource <- ptrAtOperand (prefix ++ ".source.next") source' byteCountOperand
            nextDest <- ptrAtOperand (prefix ++ ".dest.next") dest' byteCountOperand
            emitStore LLVMPtr nextSource sourceSlot
            emitStore LLVMPtr nextDest destSlot
            finishCurrentBlock (LLVMBr loopHeader)
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      source <- emitAssign "strreplace.source" LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
      byte0 <- loadByte "strreplace.b0" source 0
      isNull <- emitAssign "strreplace.end" (LLVMInt 1) (LLVMICmpEq byte0 (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isNull detectAscii [(1, done)])
      startBlock detectAscii
      asciiClass <- emitAssign "strreplace.ascii.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strreplace.is.ascii" (LLVMInt 1) (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii detectTwo [(1, asciiScalar)])
      startBlock asciiScalar
      asciiValue <- extendByte "strreplace.ascii.value" byte0
      emitStore i32Ty asciiValue scalarSlot
      emitStore i64Ty (LLVMIntLiteral 64 1) byteLengthSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock detectTwo
      twoClass <- emitAssign "strreplace.two.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strreplace.is.two" (LLVMInt 1) (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo detectThree [(1, twoByteScalar)])
      startBlock twoByteScalar
      twoByte0Masked <- emitAssign "strreplace.two.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x1F))
      twoByte0Value <- extendByte "strreplace.two.b0.value" twoByte0Masked
      twoByte0Shifted <- emitAssign "strreplace.two.b0.shifted" i32Ty (LLVMShl twoByte0Value (LLVMIntLiteral 32 6))
      twoByte1 <- loadByte "strreplace.two.b1" source 1
      twoByte1Masked <- emitAssign "strreplace.two.b1.masked" i8Ty (LLVMAnd twoByte1 (LLVMIntLiteral 8 0x3F))
      twoByte1Value <- extendByte "strreplace.two.b1.value" twoByte1Masked
      twoScalar <- emitAssign "strreplace.two.scalar" i32Ty (LLVMOr twoByte0Shifted twoByte1Value)
      emitStore i32Ty twoScalar scalarSlot
      emitStore i64Ty (LLVMIntLiteral 64 2) byteLengthSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock detectThree
      threeClass <- emitAssign "strreplace.three.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strreplace.is.three" (LLVMInt 1) (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree fourByteScalar [(1, threeByteScalar)])
      startBlock threeByteScalar
      threeByte0Masked <- emitAssign "strreplace.three.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x0F))
      threeByte0Value <- extendByte "strreplace.three.b0.value" threeByte0Masked
      threeByte0Shifted <- emitAssign "strreplace.three.b0.shifted" i32Ty (LLVMShl threeByte0Value (LLVMIntLiteral 32 12))
      threeByte1 <- loadByte "strreplace.three.b1" source 1
      threeByte1Masked <- emitAssign "strreplace.three.b1.masked" i8Ty (LLVMAnd threeByte1 (LLVMIntLiteral 8 0x3F))
      threeByte1Value <- extendByte "strreplace.three.b1.value" threeByte1Masked
      threeByte1Shifted <- emitAssign "strreplace.three.b1.shifted" i32Ty (LLVMShl threeByte1Value (LLVMIntLiteral 32 6))
      threePrefix <- emitAssign "strreplace.three.prefix" i32Ty (LLVMOr threeByte0Shifted threeByte1Shifted)
      threeByte2 <- loadByte "strreplace.three.b2" source 2
      threeByte2Masked <- emitAssign "strreplace.three.b2.masked" i8Ty (LLVMAnd threeByte2 (LLVMIntLiteral 8 0x3F))
      threeByte2Value <- extendByte "strreplace.three.b2.value" threeByte2Masked
      threeScalar <- emitAssign "strreplace.three.scalar" i32Ty (LLVMOr threePrefix threeByte2Value)
      emitStore i32Ty threeScalar scalarSlot
      emitStore i64Ty (LLVMIntLiteral 64 3) byteLengthSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock fourByteScalar
      fourByte0Masked <- emitAssign "strreplace.four.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x07))
      fourByte0Value <- extendByte "strreplace.four.b0.value" fourByte0Masked
      fourByte0Shifted <- emitAssign "strreplace.four.b0.shifted" i32Ty (LLVMShl fourByte0Value (LLVMIntLiteral 32 18))
      fourByte1 <- loadByte "strreplace.four.b1" source 1
      fourByte1Masked <- emitAssign "strreplace.four.b1.masked" i8Ty (LLVMAnd fourByte1 (LLVMIntLiteral 8 0x3F))
      fourByte1Value <- extendByte "strreplace.four.b1.value" fourByte1Masked
      fourByte1Shifted <- emitAssign "strreplace.four.b1.shifted" i32Ty (LLVMShl fourByte1Value (LLVMIntLiteral 32 12))
      fourPrefix <- emitAssign "strreplace.four.prefix" i32Ty (LLVMOr fourByte0Shifted fourByte1Shifted)
      fourByte2 <- loadByte "strreplace.four.b2" source 2
      fourByte2Masked <- emitAssign "strreplace.four.b2.masked" i8Ty (LLVMAnd fourByte2 (LLVMIntLiteral 8 0x3F))
      fourByte2Value <- extendByte "strreplace.four.b2.value" fourByte2Masked
      fourByte2Shifted <- emitAssign "strreplace.four.b2.shifted" i32Ty (LLVMShl fourByte2Value (LLVMIntLiteral 32 6))
      fourPrefix' <- emitAssign "strreplace.four.prefix2" i32Ty (LLVMOr fourPrefix fourByte2Shifted)
      fourByte3 <- loadByte "strreplace.four.b3" source 3
      fourByte3Masked <- emitAssign "strreplace.four.b3.masked" i8Ty (LLVMAnd fourByte3 (LLVMIntLiteral 8 0x3F))
      fourByte3Value <- extendByte "strreplace.four.b3.value" fourByte3Masked
      fourScalar <- emitAssign "strreplace.four.scalar" i32Ty (LLVMOr fourPrefix' fourByte3Value)
      emitStore i32Ty fourScalar scalarSlot
      emitStore i64Ty (LLVMIntLiteral 64 4) byteLengthSlot
      finishCurrentBlock (LLVMBr compareScalar)
      startBlock compareScalar
      scalar <- emitAssign "strreplace.scalar" i32Ty (LLVMLoad i32Ty scalarSlot)
      isMatch <- emitAssign "strreplace.match" (LLVMInt 1) (LLVMICmpEq scalar needle)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isMatch copyOriginal [(1, copyReplacement)])
      startBlock copyOriginal
      byteLength <- emitAssign "strreplace.copy.original.byte.length" i64Ty (LLVMLoad i64Ty byteLengthSlot)
      isOne <- emitAssign "strreplace.copy.original.is.one" (LLVMInt 1) (LLVMICmpEq byteLength (LLVMIntLiteral 64 1))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isOne copyOriginalDetectTwo [(1, copyOriginalOne)])
      startBlock copyOriginalDetectTwo
      isTwo' <- emitAssign "strreplace.copy.original.is.two" (LLVMInt 1) (LLVMICmpEq byteLength (LLVMIntLiteral 64 2))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo' copyOriginalDetectThree [(1, copyOriginalTwo)])
      startBlock copyOriginalDetectThree
      isThree' <- emitAssign "strreplace.copy.original.is.three" (LLVMInt 1) (LLVMICmpEq byteLength (LLVMIntLiteral 64 3))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree' copyOriginalFour [(1, copyOriginalThree)])
      startBlock copyOriginalOne
      finishOriginalCopy "strreplace.copy.original.one" 1
      startBlock copyOriginalTwo
      finishOriginalCopy "strreplace.copy.original.two" 2
      startBlock copyOriginalThree
      finishOriginalCopy "strreplace.copy.original.three" 3
      startBlock copyOriginalFour
      finishOriginalCopy "strreplace.copy.original.four" 4
      startBlock copyReplacement
      emitStore LLVMPtr replacementString replacementCursorSlot
      finishCurrentBlock (LLVMBr copyReplacementHeader)
      startBlock copyReplacementHeader
      replacementCursor <- emitAssign "strreplace.copy.replacement.cursor" LLVMPtr (LLVMLoad LLVMPtr replacementCursorSlot)
      replacementByte <- loadByte "strreplace.copy.replacement.byte" replacementCursor 0
      replacementDone <- emitAssign "strreplace.copy.replacement.end" (LLVMInt 1) (LLVMICmpEq replacementByte (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) replacementDone copyReplacementBody [(1, copyReplacementDone)])
      startBlock copyReplacementBody
      replacementDest <- emitAssign "strreplace.copy.replacement.dest" LLVMPtr (LLVMLoad LLVMPtr destSlot)
      replacementDestPtr <- ptrAt "strreplace.copy.replacement.dest.ptr" replacementDest 0
      emitStore i8Ty replacementByte replacementDestPtr
      nextReplacementCursor <- ptrAt "strreplace.copy.replacement.cursor.next" replacementCursor 1
      nextReplacementDest <- ptrAt "strreplace.copy.replacement.dest.next" replacementDest 1
      emitStore LLVMPtr nextReplacementCursor replacementCursorSlot
      emitStore LLVMPtr nextReplacementDest destSlot
      finishCurrentBlock (LLVMBr copyReplacementHeader)
      startBlock copyReplacementDone
      replacementSource <- emitAssign "strreplace.copy.replacement.source" LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
      replacementByteLength <- emitAssign "strreplace.copy.replacement.byte.length" i64Ty (LLVMLoad i64Ty byteLengthSlot)
      replacementNextSource <- ptrAtOperand "strreplace.copy.replacement.source.next" replacementSource replacementByteLength
      emitStore LLVMPtr replacementNextSource sourceSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock done
      destDone <- emitAssign "strreplace.done.dest" LLVMPtr (LLVMLoad LLVMPtr destSlot)
      terminatorPtr <- ptrAt "strreplace.done.ptr" destDone 0
      emitStore i8Ty (LLVMIntLiteral 8 0) terminatorPtr
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_replace_char lowering failed: " ++ renderBackendLLVMError err)

nativeStringReplaceFunction :: LLVMFunction
nativeStringReplaceFunction =
  case
    lowerNativeFunction runtimeStringReplaceName LLVMPtr [(LLVMPtr, "haystack"), (LLVMPtr, "needle"), (LLVMPtr, "replacement")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          needle = requireNativeParam "needle" params
          replacement = requireNativeParam "replacement" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte label curPtr offset = do
            bytePtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          ptrAt label curPtr offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
          ptrAtOperand label curPtr offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, offset)])
          copyByte label source dest offset = do
            byte <- loadByte (label ++ ".byte") source offset
            destPtr <- ptrAt (label ++ ".dest") dest offset
            emitStore i8Ty byte destPtr
          _countBytes label sourceInitial = do
            sourceSlot <- emitAssign (label ++ ".source.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            countSlot <- emitAssign (label ++ ".count.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
            emitStore LLVMPtr sourceInitial sourceSlot
            emitStore i64Ty (LLVMIntLiteral 64 0) countSlot
            header <- freshBlock (label ++ ".header")
            advance <- freshBlock (label ++ ".advance")
            done <- freshBlock (label ++ ".done")
            finishCurrentBlock (LLVMBr header)
            startBlock header
            source <- emitAssign (label ++ ".source") LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
            byte <- loadByte (label ++ ".byte") source 0
            isNull <- emitAssign (label ++ ".end") i1Ty (LLVMICmpEq byte (LLVMIntLiteral 8 0))
            finishCurrentBlock (LLVMSwitch i1Ty isNull advance [(1, done)])
            startBlock advance
            nextSource <- ptrAt (label ++ ".source.next") source 1
            count <- emitAssign (label ++ ".count") i64Ty (LLVMLoad i64Ty countSlot)
            nextCount <- emitAssign (label ++ ".count.next") i64Ty (LLVMAdd count (LLVMIntLiteral 64 1))
            emitStore LLVMPtr nextSource sourceSlot
            emitStore i64Ty nextCount countSlot
            finishCurrentBlock (LLVMBr header)
            startBlock done
            emitAssign (label ++ ".result") i64Ty (LLVMLoad i64Ty countSlot)
          copyFixedBytes label sourceInitial destInitial totalCount = do
            sourceSlot <- emitAssign (label ++ ".source.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            destSlot <- emitAssign (label ++ ".dest.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            copiedSlot <- emitAssign (label ++ ".copied.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
            emitStore LLVMPtr sourceInitial sourceSlot
            emitStore LLVMPtr destInitial destSlot
            emitStore i64Ty (LLVMIntLiteral 64 0) copiedSlot
            header <- freshBlock (label ++ ".header")
            copy <- freshBlock (label ++ ".copy")
            done <- freshBlock (label ++ ".done")
            finishCurrentBlock (LLVMBr header)
            startBlock header
            copied <- emitAssign (label ++ ".copied") i64Ty (LLVMLoad i64Ty copiedSlot)
            copiedEnough <- emitAssign (label ++ ".copied.enough") i1Ty (LLVMICmpEq copied totalCount)
            finishCurrentBlock (LLVMSwitch i1Ty copiedEnough copy [(1, done)])
            startBlock copy
            source <- emitAssign (label ++ ".source") LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
            dest <- emitAssign (label ++ ".dest") LLVMPtr (LLVMLoad LLVMPtr destSlot)
            byte <- loadByte (label ++ ".byte") source 0
            destPtr <- ptrAt (label ++ ".dest.ptr") dest 0
            emitStore i8Ty byte destPtr
            nextSource <- ptrAt (label ++ ".source.next") source 1
            nextDest <- ptrAt (label ++ ".dest.next") dest 1
            nextCopied <- emitAssign (label ++ ".copied.next") i64Ty (LLVMAdd copied (LLVMIntLiteral 64 1))
            emitStore LLVMPtr nextSource sourceSlot
            emitStore LLVMPtr nextDest destSlot
            emitStore i64Ty nextCopied copiedSlot
            finishCurrentBlock (LLVMBr header)
            startBlock done
            emitAssign (label ++ ".dest.result") LLVMPtr (LLVMLoad LLVMPtr destSlot)
      returnOriginal <- freshBlock "strreplace.return.original"
      countInit <- freshBlock "strreplace.count.init"
      haystackBytes <-
        emitAssign
          "strreplace.haystack.bytes"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, haystack)])
      needleBytes <-
        emitAssign
          "strreplace.needle.bytes"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, needle)])
      replacementBytes <-
        emitAssign
          "strreplace.replacement.bytes"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, replacement)])
      needleEmpty <- emitAssign "strreplace.needle.empty" i1Ty (LLVMICmpEq needleBytes (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMSwitch i1Ty needleEmpty countInit [(1, returnOriginal)])
      startBlock returnOriginal
      finishCurrentBlock (LLVMRet LLVMPtr haystack)
      startBlock countInit
      scanSourceSlot <- emitAssign "strreplace.scan.source.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      scanSourceOffsetSlot <- emitAssign "strreplace.scan.source.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      scanResultBytesSlot <- emitAssign "strreplace.scan.result.bytes.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      scanMatchHaystackSlot <- emitAssign "strreplace.scan.match.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      scanMatchNeedleSlot <- emitAssign "strreplace.scan.match.needle.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      scanMatchHaystackOffsetSlot <- emitAssign "strreplace.scan.match.haystack.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      scanMatchNeedleOffsetSlot <- emitAssign "strreplace.scan.match.needle.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore LLVMPtr haystack scanSourceSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) scanSourceOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) scanResultBytesSlot
      scanHeader <- freshBlock "strreplace.scan.header"
      scanTryMatch <- freshBlock "strreplace.scan.try-match"
      scanMatchHeader <- freshBlock "strreplace.scan.match.header"
      scanMatchHaystackEnd <- freshBlock "strreplace.scan.match.haystack-end"
      scanMatchCompare <- freshBlock "strreplace.scan.match.compare"
      scanMatchAdvance <- freshBlock "strreplace.scan.match.advance"
      scanMatchFound <- freshBlock "strreplace.scan.match.found"
      scanNoMatchDetectAscii <- freshBlock "strreplace.scan.nomatch.detect.ascii"
      scanNoMatchOne <- freshBlock "strreplace.scan.nomatch.one"
      scanNoMatchDetectTwo <- freshBlock "strreplace.scan.nomatch.detect.two"
      scanNoMatchTwo <- freshBlock "strreplace.scan.nomatch.two"
      scanNoMatchDetectThree <- freshBlock "strreplace.scan.nomatch.detect.three"
      scanNoMatchThree <- freshBlock "strreplace.scan.nomatch.three"
      scanNoMatchFour <- freshBlock "strreplace.scan.nomatch.four"
      allocateResult <- freshBlock "strreplace.allocate"
      let finishScanAdvance label resultAmount sourceAmount nextSource = do
            currentBytes <- emitAssign (label ++ ".result.bytes") i64Ty (LLVMLoad i64Ty scanResultBytesSlot)
            nextBytes <- emitAssign (label ++ ".result.bytes.next") i64Ty (LLVMAdd currentBytes resultAmount)
            sourceOffset <- emitAssign (label ++ ".source.offset") i64Ty (LLVMLoad i64Ty scanSourceOffsetSlot)
            nextSourceOffset <- emitAssign (label ++ ".source.offset.next") i64Ty (LLVMAdd sourceOffset sourceAmount)
            emitStore i64Ty nextBytes scanResultBytesSlot
            emitStore LLVMPtr nextSource scanSourceSlot
            emitStore i64Ty nextSourceOffset scanSourceOffsetSlot
            finishCurrentBlock (LLVMBr scanHeader)
          finishScanScalar label byteCount candidate = do
            nextSource <- ptrAt (label ++ ".source.next") candidate byteCount
            let byteCountOperand = LLVMIntLiteral 64 (toInteger byteCount)
            finishScanAdvance label byteCountOperand byteCountOperand nextSource
      finishCurrentBlock (LLVMBr scanHeader)
      startBlock scanHeader
      scanCandidate <- emitAssign "strreplace.scan.candidate" LLVMPtr (LLVMLoad LLVMPtr scanSourceSlot)
      scanCandidateOffset <- emitAssign "strreplace.scan.candidate.offset" i64Ty (LLVMLoad i64Ty scanSourceOffsetSlot)
      scanCandidateEnd <- emitAssign "strreplace.scan.candidate.end" i1Ty (LLVMICmpEq scanCandidateOffset haystackBytes)
      finishCurrentBlock (LLVMSwitch i1Ty scanCandidateEnd scanTryMatch [(1, allocateResult)])
      startBlock scanTryMatch
      scanCandidateByte <- loadByte "strreplace.scan.candidate.byte" scanCandidate 0
      emitStore LLVMPtr scanCandidate scanMatchHaystackSlot
      emitStore LLVMPtr needle scanMatchNeedleSlot
      emitStore i64Ty scanCandidateOffset scanMatchHaystackOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) scanMatchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr scanMatchHeader)
      startBlock scanMatchHeader
      scanNeedleCursor <- emitAssign "strreplace.scan.match.needle" LLVMPtr (LLVMLoad LLVMPtr scanMatchNeedleSlot)
      scanNeedleOffset <- emitAssign "strreplace.scan.match.needle.offset" i64Ty (LLVMLoad i64Ty scanMatchNeedleOffsetSlot)
      scanNeedleDone <- emitAssign "strreplace.scan.match.needle.done" i1Ty (LLVMICmpEq scanNeedleOffset needleBytes)
      finishCurrentBlock (LLVMSwitch i1Ty scanNeedleDone scanMatchHaystackEnd [(1, scanMatchFound)])
      startBlock scanMatchHaystackEnd
      scanHaystackCursor <- emitAssign "strreplace.scan.match.haystack" LLVMPtr (LLVMLoad LLVMPtr scanMatchHaystackSlot)
      scanHaystackOffset <- emitAssign "strreplace.scan.match.haystack.offset" i64Ty (LLVMLoad i64Ty scanMatchHaystackOffsetSlot)
      scanHaystackDone <- emitAssign "strreplace.scan.match.haystack.done" i1Ty (LLVMICmpEq scanHaystackOffset haystackBytes)
      finishCurrentBlock (LLVMSwitch i1Ty scanHaystackDone scanMatchCompare [(1, scanNoMatchDetectAscii)])
      startBlock scanMatchCompare
      scanNeedleByte <- loadByte "strreplace.scan.match.needle.byte" scanNeedleCursor 0
      scanHaystackByte <- loadByte "strreplace.scan.match.haystack.byte" scanHaystackCursor 0
      scanBytesMatch <- emitAssign "strreplace.scan.match.bytes" i1Ty (LLVMICmpEq scanHaystackByte scanNeedleByte)
      finishCurrentBlock (LLVMSwitch i1Ty scanBytesMatch scanNoMatchDetectAscii [(1, scanMatchAdvance)])
      startBlock scanMatchAdvance
      scanNextHaystack <- ptrAt "strreplace.scan.match.haystack.next" scanHaystackCursor 1
      scanNextNeedle <- ptrAt "strreplace.scan.match.needle.next" scanNeedleCursor 1
      scanNextHaystackOffset <- emitAssign "strreplace.scan.match.haystack.offset.next" i64Ty (LLVMAdd scanHaystackOffset (LLVMIntLiteral 64 1))
      scanNextNeedleOffset <- emitAssign "strreplace.scan.match.needle.offset.next" i64Ty (LLVMAdd scanNeedleOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr scanNextHaystack scanMatchHaystackSlot
      emitStore LLVMPtr scanNextNeedle scanMatchNeedleSlot
      emitStore i64Ty scanNextHaystackOffset scanMatchHaystackOffsetSlot
      emitStore i64Ty scanNextNeedleOffset scanMatchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr scanMatchHeader)
      startBlock scanMatchFound
      scanMatchNext <- ptrAtOperand "strreplace.scan.match.source.next" scanCandidate needleBytes
      finishScanAdvance "strreplace.scan.match" replacementBytes needleBytes scanMatchNext
      startBlock scanNoMatchDetectAscii
      scanAsciiClass <- emitAssign "strreplace.scan.nomatch.ascii.class" i8Ty (LLVMAnd scanCandidateByte (LLVMIntLiteral 8 0x80))
      scanIsAscii <- emitAssign "strreplace.scan.nomatch.is.ascii" i1Ty (LLVMICmpEq scanAsciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty scanIsAscii scanNoMatchDetectTwo [(1, scanNoMatchOne)])
      startBlock scanNoMatchOne
      finishScanScalar "strreplace.scan.nomatch.one" 1 scanCandidate
      startBlock scanNoMatchDetectTwo
      scanTwoClass <- emitAssign "strreplace.scan.nomatch.two.class" i8Ty (LLVMAnd scanCandidateByte (LLVMIntLiteral 8 0xE0))
      scanIsTwo <- emitAssign "strreplace.scan.nomatch.is.two" i1Ty (LLVMICmpEq scanTwoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch i1Ty scanIsTwo scanNoMatchDetectThree [(1, scanNoMatchTwo)])
      startBlock scanNoMatchTwo
      finishScanScalar "strreplace.scan.nomatch.two" 2 scanCandidate
      startBlock scanNoMatchDetectThree
      scanThreeClass <- emitAssign "strreplace.scan.nomatch.three.class" i8Ty (LLVMAnd scanCandidateByte (LLVMIntLiteral 8 0xF0))
      scanIsThree <- emitAssign "strreplace.scan.nomatch.is.three" i1Ty (LLVMICmpEq scanThreeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch i1Ty scanIsThree scanNoMatchFour [(1, scanNoMatchThree)])
      startBlock scanNoMatchThree
      finishScanScalar "strreplace.scan.nomatch.three" 3 scanCandidate
      startBlock scanNoMatchFour
      finishScanScalar "strreplace.scan.nomatch.four" 4 scanCandidate
      startBlock allocateResult
      resultBytes <- emitAssign "strreplace.result.bytes" i64Ty (LLVMLoad i64Ty scanResultBytesSlot)
      allocationSize <- emitAssign "strreplace.allocation.size" i64Ty (LLVMAdd resultBytes (LLVMIntLiteral 64 1))
      result <- emitAssign "strreplace.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, allocationSize)])
      emitSourceSlot <- emitAssign "strreplace.emit.source.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitSourceOffsetSlot <- emitAssign "strreplace.emit.source.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitDestSlot <- emitAssign "strreplace.emit.dest.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitMatchHaystackSlot <- emitAssign "strreplace.emit.match.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitMatchNeedleSlot <- emitAssign "strreplace.emit.match.needle.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitMatchHaystackOffsetSlot <- emitAssign "strreplace.emit.match.haystack.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitMatchNeedleOffsetSlot <- emitAssign "strreplace.emit.match.needle.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore LLVMPtr haystack emitSourceSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) emitSourceOffsetSlot
      emitStore LLVMPtr result emitDestSlot
      emitHeader <- freshBlock "strreplace.emit.header"
      emitTryMatch <- freshBlock "strreplace.emit.try-match"
      emitMatchHeader <- freshBlock "strreplace.emit.match.header"
      emitMatchHaystackEnd <- freshBlock "strreplace.emit.match.haystack-end"
      emitMatchCompare <- freshBlock "strreplace.emit.match.compare"
      emitMatchAdvance <- freshBlock "strreplace.emit.match.advance"
      emitMatchFound <- freshBlock "strreplace.emit.match.found"
      emitNoMatchDetectAscii <- freshBlock "strreplace.emit.nomatch.detect.ascii"
      emitNoMatchOne <- freshBlock "strreplace.emit.nomatch.one"
      emitNoMatchDetectTwo <- freshBlock "strreplace.emit.nomatch.detect.two"
      emitNoMatchTwo <- freshBlock "strreplace.emit.nomatch.two"
      emitNoMatchDetectThree <- freshBlock "strreplace.emit.nomatch.detect.three"
      emitNoMatchThree <- freshBlock "strreplace.emit.nomatch.three"
      emitNoMatchFour <- freshBlock "strreplace.emit.nomatch.four"
      done <- freshBlock "strreplace.done"
      let finishEmitScalar label byteCount source dest = do
            mapM_ (copyByte label source dest) [0 .. byteCount - 1]
            nextSource <- ptrAt (label ++ ".source.next") source byteCount
            nextDest <- ptrAt (label ++ ".dest.next") dest byteCount
            sourceOffset <- emitAssign (label ++ ".source.offset") i64Ty (LLVMLoad i64Ty emitSourceOffsetSlot)
            nextSourceOffset <- emitAssign (label ++ ".source.offset.next") i64Ty (LLVMAdd sourceOffset (LLVMIntLiteral 64 (toInteger byteCount)))
            emitStore LLVMPtr nextSource emitSourceSlot
            emitStore LLVMPtr nextDest emitDestSlot
            emitStore i64Ty nextSourceOffset emitSourceOffsetSlot
            finishCurrentBlock (LLVMBr emitHeader)
      finishCurrentBlock (LLVMBr emitHeader)
      startBlock emitHeader
      emitSource <- emitAssign "strreplace.emit.source" LLVMPtr (LLVMLoad LLVMPtr emitSourceSlot)
      emitDest <- emitAssign "strreplace.emit.dest" LLVMPtr (LLVMLoad LLVMPtr emitDestSlot)
      emitSourceOffset <- emitAssign "strreplace.emit.source.offset" i64Ty (LLVMLoad i64Ty emitSourceOffsetSlot)
      emitCandidateEnd <- emitAssign "strreplace.emit.candidate.end" i1Ty (LLVMICmpEq emitSourceOffset haystackBytes)
      finishCurrentBlock (LLVMSwitch i1Ty emitCandidateEnd emitTryMatch [(1, done)])
      startBlock emitTryMatch
      emitCandidateByte <- loadByte "strreplace.emit.candidate.byte" emitSource 0
      emitStore LLVMPtr emitSource emitMatchHaystackSlot
      emitStore LLVMPtr needle emitMatchNeedleSlot
      emitStore i64Ty emitSourceOffset emitMatchHaystackOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) emitMatchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr emitMatchHeader)
      startBlock emitMatchHeader
      emitNeedleCursor <- emitAssign "strreplace.emit.match.needle" LLVMPtr (LLVMLoad LLVMPtr emitMatchNeedleSlot)
      emitNeedleOffset <- emitAssign "strreplace.emit.match.needle.offset" i64Ty (LLVMLoad i64Ty emitMatchNeedleOffsetSlot)
      emitNeedleDone <- emitAssign "strreplace.emit.match.needle.done" i1Ty (LLVMICmpEq emitNeedleOffset needleBytes)
      finishCurrentBlock (LLVMSwitch i1Ty emitNeedleDone emitMatchHaystackEnd [(1, emitMatchFound)])
      startBlock emitMatchHaystackEnd
      emitHaystackCursor <- emitAssign "strreplace.emit.match.haystack" LLVMPtr (LLVMLoad LLVMPtr emitMatchHaystackSlot)
      emitHaystackOffset <- emitAssign "strreplace.emit.match.haystack.offset" i64Ty (LLVMLoad i64Ty emitMatchHaystackOffsetSlot)
      emitHaystackDone <- emitAssign "strreplace.emit.match.haystack.done" i1Ty (LLVMICmpEq emitHaystackOffset haystackBytes)
      finishCurrentBlock (LLVMSwitch i1Ty emitHaystackDone emitMatchCompare [(1, emitNoMatchDetectAscii)])
      startBlock emitMatchCompare
      emitNeedleByte <- loadByte "strreplace.emit.match.needle.byte" emitNeedleCursor 0
      emitHaystackByte <- loadByte "strreplace.emit.match.haystack.byte" emitHaystackCursor 0
      emitBytesMatch <- emitAssign "strreplace.emit.match.bytes" i1Ty (LLVMICmpEq emitHaystackByte emitNeedleByte)
      finishCurrentBlock (LLVMSwitch i1Ty emitBytesMatch emitNoMatchDetectAscii [(1, emitMatchAdvance)])
      startBlock emitMatchAdvance
      emitNextHaystack <- ptrAt "strreplace.emit.match.haystack.next" emitHaystackCursor 1
      emitNextNeedle <- ptrAt "strreplace.emit.match.needle.next" emitNeedleCursor 1
      emitNextHaystackOffset <- emitAssign "strreplace.emit.match.haystack.offset.next" i64Ty (LLVMAdd emitHaystackOffset (LLVMIntLiteral 64 1))
      emitNextNeedleOffset <- emitAssign "strreplace.emit.match.needle.offset.next" i64Ty (LLVMAdd emitNeedleOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr emitNextHaystack emitMatchHaystackSlot
      emitStore LLVMPtr emitNextNeedle emitMatchNeedleSlot
      emitStore i64Ty emitNextHaystackOffset emitMatchHaystackOffsetSlot
      emitStore i64Ty emitNextNeedleOffset emitMatchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr emitMatchHeader)
      startBlock emitMatchFound
      emitDestAtMatch <- emitAssign "strreplace.emit.match.dest" LLVMPtr (LLVMLoad LLVMPtr emitDestSlot)
      afterReplacement <- copyFixedBytes "strreplace.emit.copy.replacement" replacement emitDestAtMatch replacementBytes
      emitSourceAtMatch <- emitAssign "strreplace.emit.match.source" LLVMPtr (LLVMLoad LLVMPtr emitSourceSlot)
      emitMatchNextSource <- ptrAtOperand "strreplace.emit.match.source.next" emitSourceAtMatch needleBytes
      emitSourceOffsetAtMatch <- emitAssign "strreplace.emit.match.source.offset" i64Ty (LLVMLoad i64Ty emitSourceOffsetSlot)
      emitMatchNextSourceOffset <- emitAssign "strreplace.emit.match.source.offset.next" i64Ty (LLVMAdd emitSourceOffsetAtMatch needleBytes)
      emitStore LLVMPtr emitMatchNextSource emitSourceSlot
      emitStore i64Ty emitMatchNextSourceOffset emitSourceOffsetSlot
      emitStore LLVMPtr afterReplacement emitDestSlot
      finishCurrentBlock (LLVMBr emitHeader)
      startBlock emitNoMatchDetectAscii
      emitAsciiClass <- emitAssign "strreplace.emit.nomatch.ascii.class" i8Ty (LLVMAnd emitCandidateByte (LLVMIntLiteral 8 0x80))
      emitIsAscii <- emitAssign "strreplace.emit.nomatch.is.ascii" i1Ty (LLVMICmpEq emitAsciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty emitIsAscii emitNoMatchDetectTwo [(1, emitNoMatchOne)])
      startBlock emitNoMatchOne
      finishEmitScalar "strreplace.emit.nomatch.one" 1 emitSource emitDest
      startBlock emitNoMatchDetectTwo
      emitTwoClass <- emitAssign "strreplace.emit.nomatch.two.class" i8Ty (LLVMAnd emitCandidateByte (LLVMIntLiteral 8 0xE0))
      emitIsTwo <- emitAssign "strreplace.emit.nomatch.is.two" i1Ty (LLVMICmpEq emitTwoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch i1Ty emitIsTwo emitNoMatchDetectThree [(1, emitNoMatchTwo)])
      startBlock emitNoMatchTwo
      finishEmitScalar "strreplace.emit.nomatch.two" 2 emitSource emitDest
      startBlock emitNoMatchDetectThree
      emitThreeClass <- emitAssign "strreplace.emit.nomatch.three.class" i8Ty (LLVMAnd emitCandidateByte (LLVMIntLiteral 8 0xF0))
      emitIsThree <- emitAssign "strreplace.emit.nomatch.is.three" i1Ty (LLVMICmpEq emitThreeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch i1Ty emitIsThree emitNoMatchFour [(1, emitNoMatchThree)])
      startBlock emitNoMatchThree
      finishEmitScalar "strreplace.emit.nomatch.three" 3 emitSource emitDest
      startBlock emitNoMatchFour
      finishEmitScalar "strreplace.emit.nomatch.four" 4 emitSource emitDest
      startBlock done
      doneDest <- emitAssign "strreplace.done.dest" LLVMPtr (LLVMLoad LLVMPtr emitDestSlot)
      donePtr <- ptrAt "strreplace.done.ptr" doneDest 0
      emitStore i8Ty (LLVMIntLiteral 8 0) donePtr
      _ <-
        emitAssign
          "strreplace.register.length"
          (LLVMInt 32)
          (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, resultBytes)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_replace lowering failed: " ++ renderBackendLLVMError err)

nativeStringIndexOfCharFunction :: LLVMFunction
nativeStringIndexOfCharFunction =
  case
    lowerNativeFunction runtimeStringIndexOfCharName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 32, "needle")] $ \params -> do
      let value = requireNativeParam "value" params
          needle = requireNativeParam "needle" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          bytePtrAt label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
          loadByteAt label base offset = do
            ptr <- bytePtrAt (label ++ ".ptr") base offset
            emitAssign label i8Ty (LLVMLoad i8Ty ptr)
          advancePtr label base offset =
            bytePtrAt label base offset
          zextByte label byte =
            emitAssign label i32Ty (LLVMZext byte i32Ty)
          shiftedPayload label byte mask shift = do
            masked <- emitAssign (label ++ ".masked") i8Ty (LLVMAnd byte (LLVMIntLiteral 8 mask))
            extended <- zextByte (label ++ ".i32") masked
            if shift == 0
              then pure extended
              else emitAssign (label ++ ".shifted") i32Ty (LLVMShl extended (LLVMIntLiteral 32 shift))
          allocateNone label = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 0)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 0) tagPtr
            pure cell
          allocateSome label index = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 1)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 1) tagPtr
            valuePtr <- emitGep (label ++ ".value") cell (constructorFieldOffset 0)
            emitStore i64Ty index valuePtr
            pure cell
          storeDecoded scalarSlot nextSlot scalarByteLengthSlot compareBlock scalar nextPtr byteLength = do
            emitStore i32Ty scalar scalarSlot
            emitStore LLVMPtr nextPtr nextSlot
            emitStore i64Ty (LLVMIntLiteral 64 byteLength) scalarByteLengthSlot
            finishCurrentBlock (LLVMBr compareBlock)
      curSlot <- emitAssign "strindex.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      indexSlot <- emitAssign "strindex.index.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      byteOffsetSlot <- emitAssign "strindex.byte.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      scalarSlot <- emitAssign "strindex.scalar.slot" LLVMPtr (LLVMAlloca i32Ty (LLVMIntLiteral 64 1))
      scalarByteLengthSlot <- emitAssign "strindex.scalar.byte.length.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      nextSlot <- emitAssign "strindex.next.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      byteLength <-
        emitAssign
          "strindex.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      emitStore LLVMPtr value curSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) indexSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) byteOffsetSlot
      loopHeader <- freshBlock "strindex.header"
      decode <- freshBlock "strindex.decode"
      ascii <- freshBlock "strindex.ascii"
      detectTwo <- freshBlock "strindex.detect.two"
      twoByteScalar <- freshBlock "strindex.two"
      detectThree <- freshBlock "strindex.detect.three"
      threeByteScalar <- freshBlock "strindex.three"
      fourByteScalar <- freshBlock "strindex.four"
      compareScalar <- freshBlock "strindex.compare"
      match <- freshBlock "strindex.match"
      advance <- freshBlock "strindex.advance"
      absent <- freshBlock "strindex.absent"
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      cur <- emitAssign "strindex.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      byteOffset <- emitAssign "strindex.byte.offset" i64Ty (LLVMLoad i64Ty byteOffsetSlot)
      complete <- emitAssign "strindex.complete" i1Ty (LLVMICmpEq byteOffset byteLength)
      finishCurrentBlock (LLVMSwitch i1Ty complete decode [(1, absent)])
      startBlock decode
      byte0 <- loadByteAt "strindex.byte0" cur 0
      asciiClass <- emitAssign "strindex.ascii.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strindex.is.ascii" i1Ty (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty isAscii detectTwo [(1, ascii)])
      startBlock ascii
      asciiScalar <- zextByte "strindex.ascii.scalar" byte0
      asciiNext <- advancePtr "strindex.ascii.next" cur 1
      storeDecoded scalarSlot nextSlot scalarByteLengthSlot compareScalar asciiScalar asciiNext 1
      startBlock detectTwo
      twoClass <- emitAssign "strindex.two.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strindex.is.two" i1Ty (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch i1Ty isTwo detectThree [(1, twoByteScalar)])
      startBlock twoByteScalar
      twoByte1 <- loadByteAt "strindex.two.b1" cur 1
      twoByte0Shifted <- shiftedPayload "strindex.two.b0" byte0 0x1F 6
      twoByte1Value <- shiftedPayload "strindex.two.b1.payload" twoByte1 0x3F 0
      twoScalar <- emitAssign "strindex.two.scalar" i32Ty (LLVMOr twoByte0Shifted twoByte1Value)
      twoNext <- advancePtr "strindex.two.next" cur 2
      storeDecoded scalarSlot nextSlot scalarByteLengthSlot compareScalar twoScalar twoNext 2
      startBlock detectThree
      threeClass <- emitAssign "strindex.three.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strindex.is.three" i1Ty (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch i1Ty isThree fourByteScalar [(1, threeByteScalar)])
      startBlock threeByteScalar
      threeByte1 <- loadByteAt "strindex.three.b1" cur 1
      threeByte2 <- loadByteAt "strindex.three.b2" cur 2
      threeByte0Shifted <- shiftedPayload "strindex.three.b0" byte0 0x0F 12
      threeByte1Shifted <- shiftedPayload "strindex.three.b1.payload" threeByte1 0x3F 6
      threePrefix <- emitAssign "strindex.three.prefix" i32Ty (LLVMOr threeByte0Shifted threeByte1Shifted)
      threeByte2Value <- shiftedPayload "strindex.three.b2.payload" threeByte2 0x3F 0
      threeScalar <- emitAssign "strindex.three.scalar" i32Ty (LLVMOr threePrefix threeByte2Value)
      threeNext <- advancePtr "strindex.three.next" cur 3
      storeDecoded scalarSlot nextSlot scalarByteLengthSlot compareScalar threeScalar threeNext 3
      startBlock fourByteScalar
      fourByte1 <- loadByteAt "strindex.four.b1" cur 1
      fourByte2 <- loadByteAt "strindex.four.b2" cur 2
      fourByte3 <- loadByteAt "strindex.four.b3" cur 3
      fourByte0Shifted <- shiftedPayload "strindex.four.b0" byte0 0x07 18
      fourByte1Shifted <- shiftedPayload "strindex.four.b1.payload" fourByte1 0x3F 12
      fourPrefix0 <- emitAssign "strindex.four.prefix0" i32Ty (LLVMOr fourByte0Shifted fourByte1Shifted)
      fourByte2Shifted <- shiftedPayload "strindex.four.b2.payload" fourByte2 0x3F 6
      fourPrefix1 <- emitAssign "strindex.four.prefix1" i32Ty (LLVMOr fourPrefix0 fourByte2Shifted)
      fourByte3Value <- shiftedPayload "strindex.four.b3.payload" fourByte3 0x3F 0
      fourScalar <- emitAssign "strindex.four.scalar" i32Ty (LLVMOr fourPrefix1 fourByte3Value)
      fourNext <- advancePtr "strindex.four.next" cur 4
      storeDecoded scalarSlot nextSlot scalarByteLengthSlot compareScalar fourScalar fourNext 4
      startBlock compareScalar
      scalar <- emitAssign "strindex.scalar" i32Ty (LLVMLoad i32Ty scalarSlot)
      isMatch <- emitAssign "strindex.match.value" i1Ty (LLVMICmpEq scalar needle)
      finishCurrentBlock (LLVMSwitch i1Ty isMatch advance [(1, match)])
      startBlock match
      index <- emitAssign "strindex.result.index" i64Ty (LLVMLoad i64Ty indexSlot)
      some <- allocateSome "strindex.some" index
      finishCurrentBlock (LLVMRet LLVMPtr some)
      startBlock advance
      next <- emitAssign "strindex.advance.next" LLVMPtr (LLVMLoad LLVMPtr nextSlot)
      oldIndex <- emitAssign "strindex.advance.index" i64Ty (LLVMLoad i64Ty indexSlot)
      nextIndex <- emitAssign "strindex.advance.index.next" i64Ty (LLVMAdd oldIndex (LLVMIntLiteral 64 1))
      scalarByteLength <- emitAssign "strindex.advance.scalar.byte.length" i64Ty (LLVMLoad i64Ty scalarByteLengthSlot)
      nextByteOffset <- emitAssign "strindex.advance.byte.offset.next" i64Ty (LLVMAdd byteOffset scalarByteLength)
      emitStore LLVMPtr next curSlot
      emitStore i64Ty nextIndex indexSlot
      emitStore i64Ty nextByteOffset byteOffsetSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock absent
      none <- allocateNone "strindex.none"
      finishCurrentBlock (LLVMRet LLVMPtr none)
  of
    Right function -> function
    Left err -> error ("internal native __string_index_of_char lowering failed: " ++ renderBackendLLVMError err)

nativeStringIndexOfFunction :: LLVMFunction
nativeStringIndexOfFunction =
  case
    lowerNativeFunction runtimeStringIndexOfName LLVMPtr [(LLVMPtr, "haystack"), (LLVMPtr, "needle")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          needle = requireNativeParam "needle" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          bytePtrAt label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
          loadByteAt label base offset = do
            ptr <- bytePtrAt (label ++ ".ptr") base offset
            emitAssign label i8Ty (LLVMLoad i8Ty ptr)
          advancePtr label base offset =
            bytePtrAt label base offset
          allocateNone label = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 0)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 0) tagPtr
            pure cell
          allocateSome label index = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 1)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 1) tagPtr
            valuePtr <- emitGep (label ++ ".value") cell (constructorFieldOffset 0)
            emitStore i64Ty index valuePtr
            pure cell
      candidateSlot <- emitAssign "strindexof.candidate.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      indexSlot <- emitAssign "strindexof.index.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      candidateOffsetSlot <- emitAssign "strindexof.candidate.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      matchHaystackSlot <- emitAssign "strindexof.match.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchNeedleSlot <- emitAssign "strindexof.match.needle.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchHaystackOffsetSlot <- emitAssign "strindexof.match.haystack.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      matchNeedleOffsetSlot <- emitAssign "strindexof.match.needle.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      haystackLength <-
        emitAssign
          "strindexof.haystack.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, haystack)])
      needleLength <-
        emitAssign
          "strindexof.needle.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, needle)])
      emitStore LLVMPtr haystack candidateSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) indexSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) candidateOffsetSlot
      candidateHeader <- freshBlock "strindexof.candidate.header"
      candidateStart <- freshBlock "strindexof.candidate.start"
      matchHeader <- freshBlock "strindexof.match.header"
      matchHaystackEnd <- freshBlock "strindexof.match.haystack-end"
      matchCompare <- freshBlock "strindexof.match.compare"
      matchAdvance <- freshBlock "strindexof.match.advance"
      candidateAdvance <- freshBlock "strindexof.candidate.advance"
      advanceAscii <- freshBlock "strindexof.advance.ascii"
      advanceDetectTwo <- freshBlock "strindexof.advance.detect.two"
      advanceTwo <- freshBlock "strindexof.advance.two"
      advanceDetectThree <- freshBlock "strindexof.advance.detect.three"
      advanceThree <- freshBlock "strindexof.advance.three"
      advanceFour <- freshBlock "strindexof.advance.four"
      found <- freshBlock "strindexof.found"
      absent <- freshBlock "strindexof.absent"
      let finishCandidateAdvance label offset currentCandidate = do
            nextCandidate <- advancePtr (label ++ ".next") currentCandidate offset
            oldIndex <- emitAssign (label ++ ".index") i64Ty (LLVMLoad i64Ty indexSlot)
            nextIndex <- emitAssign (label ++ ".index.next") i64Ty (LLVMAdd oldIndex (LLVMIntLiteral 64 1))
            oldCandidateOffset <- emitAssign (label ++ ".candidate.offset") i64Ty (LLVMLoad i64Ty candidateOffsetSlot)
            nextCandidateOffset <- emitAssign (label ++ ".candidate.offset.next") i64Ty (LLVMAdd oldCandidateOffset (LLVMIntLiteral 64 offset))
            emitStore LLVMPtr nextCandidate candidateSlot
            emitStore i64Ty nextIndex indexSlot
            emitStore i64Ty nextCandidateOffset candidateOffsetSlot
            finishCurrentBlock (LLVMBr candidateHeader)
      needleEmpty <- emitAssign "strindexof.needle.empty" i1Ty (LLVMICmpEq needleLength (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMSwitch i1Ty needleEmpty candidateHeader [(1, found)])
      startBlock candidateHeader
      candidate <- emitAssign "strindexof.candidate" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      candidateOffset <- emitAssign "strindexof.candidate.offset" i64Ty (LLVMLoad i64Ty candidateOffsetSlot)
      candidateEnd <- emitAssign "strindexof.candidate.end" i1Ty (LLVMICmpEq candidateOffset haystackLength)
      finishCurrentBlock (LLVMSwitch i1Ty candidateEnd candidateStart [(1, absent)])
      startBlock candidateStart
      emitStore LLVMPtr candidate matchHaystackSlot
      emitStore LLVMPtr needle matchNeedleSlot
      emitStore i64Ty candidateOffset matchHaystackOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) matchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock matchHeader
      haystackCursor <- emitAssign "strindexof.match.haystack" LLVMPtr (LLVMLoad LLVMPtr matchHaystackSlot)
      needleCursor <- emitAssign "strindexof.match.needle" LLVMPtr (LLVMLoad LLVMPtr matchNeedleSlot)
      matchNeedleOffset <- emitAssign "strindexof.match.needle.offset" i64Ty (LLVMLoad i64Ty matchNeedleOffsetSlot)
      needleDone <- emitAssign "strindexof.match.needle.done" i1Ty (LLVMICmpEq matchNeedleOffset needleLength)
      finishCurrentBlock (LLVMSwitch i1Ty needleDone matchHaystackEnd [(1, found)])
      startBlock matchHaystackEnd
      matchHaystackOffset <- emitAssign "strindexof.match.haystack.offset" i64Ty (LLVMLoad i64Ty matchHaystackOffsetSlot)
      haystackDone <- emitAssign "strindexof.match.haystack.done" i1Ty (LLVMICmpEq matchHaystackOffset haystackLength)
      finishCurrentBlock (LLVMSwitch i1Ty haystackDone matchCompare [(1, candidateAdvance)])
      startBlock matchCompare
      needleByte <- loadByteAt "strindexof.match.needle.byte" needleCursor 0
      haystackByte <- loadByteAt "strindexof.match.haystack.byte" haystackCursor 0
      bytesMatch <- emitAssign "strindexof.match.bytes" i1Ty (LLVMICmpEq haystackByte needleByte)
      finishCurrentBlock (LLVMSwitch i1Ty bytesMatch candidateAdvance [(1, matchAdvance)])
      startBlock matchAdvance
      nextHaystack <- advancePtr "strindexof.match.haystack.next" haystackCursor 1
      nextNeedle <- advancePtr "strindexof.match.needle.next" needleCursor 1
      nextMatchHaystackOffset <- emitAssign "strindexof.match.haystack.offset.next" i64Ty (LLVMAdd matchHaystackOffset (LLVMIntLiteral 64 1))
      nextMatchNeedleOffset <- emitAssign "strindexof.match.needle.offset.next" i64Ty (LLVMAdd matchNeedleOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr nextHaystack matchHaystackSlot
      emitStore LLVMPtr nextNeedle matchNeedleSlot
      emitStore i64Ty nextMatchHaystackOffset matchHaystackOffsetSlot
      emitStore i64Ty nextMatchNeedleOffset matchNeedleOffsetSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock candidateAdvance
      currentCandidate <- emitAssign "strindexof.advance.candidate" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      advanceByte <- loadByteAt "strindexof.advance.byte" currentCandidate 0
      asciiClass <- emitAssign "strindexof.advance.ascii.class" i8Ty (LLVMAnd advanceByte (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strindexof.advance.is.ascii" i1Ty (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty isAscii advanceDetectTwo [(1, advanceAscii)])
      startBlock advanceAscii
      finishCandidateAdvance "strindexof.advance.ascii" 1 currentCandidate
      startBlock advanceDetectTwo
      twoClass <- emitAssign "strindexof.advance.two.class" i8Ty (LLVMAnd advanceByte (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strindexof.advance.is.two" i1Ty (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch i1Ty isTwo advanceDetectThree [(1, advanceTwo)])
      startBlock advanceTwo
      finishCandidateAdvance "strindexof.advance.two" 2 currentCandidate
      startBlock advanceDetectThree
      threeClass <- emitAssign "strindexof.advance.three.class" i8Ty (LLVMAnd advanceByte (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strindexof.advance.is.three" i1Ty (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch i1Ty isThree advanceFour [(1, advanceThree)])
      startBlock advanceThree
      finishCandidateAdvance "strindexof.advance.three" 3 currentCandidate
      startBlock advanceFour
      finishCandidateAdvance "strindexof.advance.four" 4 currentCandidate
      startBlock found
      index <- emitAssign "strindexof.result.index" i64Ty (LLVMLoad i64Ty indexSlot)
      some <- allocateSome "strindexof.some" index
      finishCurrentBlock (LLVMRet LLVMPtr some)
      startBlock absent
      none <- allocateNone "strindexof.none"
      finishCurrentBlock (LLVMRet LLVMPtr none)
  of
    Right function -> function
    Left err -> error ("internal native __string_index_of lowering failed: " ++ renderBackendLLVMError err)

nativeStringSplitFunction :: LLVMFunction
nativeStringSplitFunction =
  case
    lowerNativeFunction runtimeStringSplitName LLVMPtr [(LLVMPtr, "haystack"), (LLVMPtr, "delimiter")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          delimiter = requireNativeParam "delimiter" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          bytePtrAt label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
          bytePtrAtOperand label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, offset)])
          loadByteAt label base offset = do
            ptr <- bytePtrAt (label ++ ".ptr") base offset
            emitAssign label i8Ty (LLVMLoad i8Ty ptr)
          allocateNil label = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 0)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 0) tagPtr
            pure cell
          allocateCons label headValue tailValue = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 2)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 1) tagPtr
            headPtr <- emitGep (label ++ ".head") cell (constructorFieldOffset 0)
            emitStore LLVMPtr headValue headPtr
            tailPtr <- emitGep (label ++ ".tail") cell (constructorFieldOffset 1)
            emitStore LLVMPtr tailValue tailPtr
            pure cell
          _countBytes label sourceInitial = do
            sourceSlot <- emitAssign (label ++ ".source.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            countSlot <- emitAssign (label ++ ".count.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
            emitStore LLVMPtr sourceInitial sourceSlot
            emitStore i64Ty (LLVMIntLiteral 64 0) countSlot
            header <- freshBlock (label ++ ".header")
            advance <- freshBlock (label ++ ".advance")
            done <- freshBlock (label ++ ".done")
            finishCurrentBlock (LLVMBr header)
            startBlock header
            source <- emitAssign (label ++ ".source") LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
            byte <- loadByteAt (label ++ ".byte") source 0
            isNull <- emitAssign (label ++ ".end") i1Ty (LLVMICmpEq byte (LLVMIntLiteral 8 0))
            finishCurrentBlock (LLVMSwitch i1Ty isNull advance [(1, done)])
            startBlock advance
            nextSource <- bytePtrAt (label ++ ".source.next") source 1
            count <- emitAssign (label ++ ".count") i64Ty (LLVMLoad i64Ty countSlot)
            nextCount <- emitAssign (label ++ ".count.next") i64Ty (LLVMAdd count (LLVMIntLiteral 64 1))
            emitStore LLVMPtr nextSource sourceSlot
            emitStore i64Ty nextCount countSlot
            finishCurrentBlock (LLVMBr header)
            startBlock done
            emitAssign (label ++ ".result") i64Ty (LLVMLoad i64Ty countSlot)
          copyRangeString label sourceInitial endPtr = do
            countSourceSlot <- emitAssign (label ++ ".count.source.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            countSlot <- emitAssign (label ++ ".count.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
            emitStore LLVMPtr sourceInitial countSourceSlot
            emitStore i64Ty (LLVMIntLiteral 64 0) countSlot
            countHeader <- freshBlock (label ++ ".count.header")
            countAdvance <- freshBlock (label ++ ".count.advance")
            countDone <- freshBlock (label ++ ".count.done")
            finishCurrentBlock (LLVMBr countHeader)
            startBlock countHeader
            countSource <- emitAssign (label ++ ".count.source") LLVMPtr (LLVMLoad LLVMPtr countSourceSlot)
            countAtEnd <- emitAssign (label ++ ".count.at.end") i1Ty (LLVMICmpEq countSource endPtr)
            finishCurrentBlock (LLVMSwitch i1Ty countAtEnd countAdvance [(1, countDone)])
            startBlock countAdvance
            nextCountSource <- bytePtrAt (label ++ ".count.source.next") countSource 1
            count <- emitAssign (label ++ ".count") i64Ty (LLVMLoad i64Ty countSlot)
            nextCount <- emitAssign (label ++ ".count.next") i64Ty (LLVMAdd count (LLVMIntLiteral 64 1))
            emitStore LLVMPtr nextCountSource countSourceSlot
            emitStore i64Ty nextCount countSlot
            finishCurrentBlock (LLVMBr countHeader)
            startBlock countDone
            totalBytes <- emitAssign (label ++ ".bytes") i64Ty (LLVMLoad i64Ty countSlot)
            allocationSize <- emitAssign (label ++ ".allocation.size") i64Ty (LLVMAdd totalBytes (LLVMIntLiteral 64 1))
            result <- emitAssign (label ++ ".result") LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, allocationSize)])
            copySourceSlot <- emitAssign (label ++ ".copy.source.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            destSlot <- emitAssign (label ++ ".copy.dest.slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
            copiedSlot <- emitAssign (label ++ ".copied.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
            emitStore LLVMPtr sourceInitial copySourceSlot
            emitStore LLVMPtr result destSlot
            emitStore i64Ty (LLVMIntLiteral 64 0) copiedSlot
            copyHeader <- freshBlock (label ++ ".copy.header")
            copyBody <- freshBlock (label ++ ".copy.body")
            copyDone <- freshBlock (label ++ ".copy.done")
            finishCurrentBlock (LLVMBr copyHeader)
            startBlock copyHeader
            copied <- emitAssign (label ++ ".copied") i64Ty (LLVMLoad i64Ty copiedSlot)
            copiedAll <- emitAssign (label ++ ".copied.all") i1Ty (LLVMICmpEq copied totalBytes)
            finishCurrentBlock (LLVMSwitch i1Ty copiedAll copyBody [(1, copyDone)])
            startBlock copyBody
            copySource <- emitAssign (label ++ ".copy.source") LLVMPtr (LLVMLoad LLVMPtr copySourceSlot)
            dest <- emitAssign (label ++ ".copy.dest") LLVMPtr (LLVMLoad LLVMPtr destSlot)
            byte <- loadByteAt (label ++ ".copy.byte") copySource 0
            destPtr <- bytePtrAt (label ++ ".copy.dest.ptr") dest 0
            emitStore i8Ty byte destPtr
            nextCopySource <- bytePtrAt (label ++ ".copy.source.next") copySource 1
            nextDest <- bytePtrAt (label ++ ".copy.dest.next") dest 1
            nextCopied <- emitAssign (label ++ ".copied.next") i64Ty (LLVMAdd copied (LLVMIntLiteral 64 1))
            emitStore LLVMPtr nextCopySource copySourceSlot
            emitStore LLVMPtr nextDest destSlot
            emitStore i64Ty nextCopied copiedSlot
            finishCurrentBlock (LLVMBr copyHeader)
            startBlock copyDone
            doneDest <- emitAssign (label ++ ".done.dest") LLVMPtr (LLVMLoad LLVMPtr destSlot)
            donePtr <- bytePtrAt (label ++ ".done.ptr") doneDest 0
            emitStore i8Ty (LLVMIntLiteral 8 0) donePtr
            _ <-
              emitAssign
                (label ++ ".register.length")
                (LLVMInt 32)
                (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, totalBytes)])
            pure result
      emptyDelimiter <- freshBlock "strsplit.empty-delimiter"
      initSplit <- freshBlock "strsplit.init"
      haystackBytes <-
        emitAssign
          "strsplit.haystack.bytes"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, haystack)])
      delimiterBytes <-
        emitAssign
          "strsplit.delimiter.bytes"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, delimiter)])
      delimiterIsEmpty <- emitAssign "strsplit.delimiter.empty" i1Ty (LLVMICmpEq delimiterBytes (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMSwitch i1Ty delimiterIsEmpty initSplit [(1, emptyDelimiter)])
      startBlock emptyDelimiter
      singletonNil <- allocateNil "strsplit.empty.nil"
      singleton <- allocateCons "strsplit.empty.cons" haystack singletonNil
      finishCurrentBlock (LLVMRet LLVMPtr singleton)
      startBlock initSplit
      segmentStartSlot <- emitAssign "strsplit.segment.start.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      candidateSlot <- emitAssign "strsplit.candidate.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      candidateOffsetSlot <- emitAssign "strsplit.candidate.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      accSlot <- emitAssign "strsplit.acc.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchHaystackSlot <- emitAssign "strsplit.match.haystack.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchDelimiterSlot <- emitAssign "strsplit.match.delimiter.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      matchHaystackOffsetSlot <- emitAssign "strsplit.match.haystack.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      matchDelimiterOffsetSlot <- emitAssign "strsplit.match.delimiter.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      initialNil <- allocateNil "strsplit.initial.nil"
      emitStore LLVMPtr haystack segmentStartSlot
      emitStore LLVMPtr haystack candidateSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) candidateOffsetSlot
      emitStore LLVMPtr initialNil accSlot
      candidateHeader <- freshBlock "strsplit.candidate.header"
      tryMatch <- freshBlock "strsplit.try-match"
      matchHeader <- freshBlock "strsplit.match.header"
      matchHaystackEnd <- freshBlock "strsplit.match.haystack-end"
      matchCompare <- freshBlock "strsplit.match.compare"
      matchAdvance <- freshBlock "strsplit.match.advance"
      matchFound <- freshBlock "strsplit.match.found"
      candidateAdvance <- freshBlock "strsplit.candidate.advance"
      advanceAscii <- freshBlock "strsplit.advance.ascii"
      advanceDetectTwo <- freshBlock "strsplit.advance.detect.two"
      advanceTwo <- freshBlock "strsplit.advance.two"
      advanceDetectThree <- freshBlock "strsplit.advance.detect.three"
      advanceThree <- freshBlock "strsplit.advance.three"
      advanceFour <- freshBlock "strsplit.advance.four"
      finalSegment <- freshBlock "strsplit.final-segment"
      reverseInit <- freshBlock "strsplit.reverse.init"
      let pushSegment label segmentEnd nextSegmentStart nextCandidate nextCandidateOffset = do
            segmentStart <- emitAssign (label ++ ".segment.start") LLVMPtr (LLVMLoad LLVMPtr segmentStartSlot)
            segment <- copyRangeString (label ++ ".segment") segmentStart segmentEnd
            acc <- emitAssign (label ++ ".acc") LLVMPtr (LLVMLoad LLVMPtr accSlot)
            cons <- allocateCons (label ++ ".cons") segment acc
            emitStore LLVMPtr cons accSlot
            emitStore LLVMPtr nextSegmentStart segmentStartSlot
            emitStore LLVMPtr nextCandidate candidateSlot
            emitStore i64Ty nextCandidateOffset candidateOffsetSlot
          finishCandidateAdvance label offset currentCandidate = do
            nextCandidate <- bytePtrAt (label ++ ".next") currentCandidate offset
            candidateOffset <- emitAssign (label ++ ".candidate.offset") i64Ty (LLVMLoad i64Ty candidateOffsetSlot)
            nextCandidateOffset <- emitAssign (label ++ ".candidate.offset.next") i64Ty (LLVMAdd candidateOffset (LLVMIntLiteral 64 offset))
            emitStore LLVMPtr nextCandidate candidateSlot
            emitStore i64Ty nextCandidateOffset candidateOffsetSlot
            finishCurrentBlock (LLVMBr candidateHeader)
      finishCurrentBlock (LLVMBr candidateHeader)
      startBlock candidateHeader
      candidate <- emitAssign "strsplit.candidate" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      candidateOffset <- emitAssign "strsplit.candidate.offset" i64Ty (LLVMLoad i64Ty candidateOffsetSlot)
      candidateEnd <- emitAssign "strsplit.candidate.end" i1Ty (LLVMICmpEq candidateOffset haystackBytes)
      finishCurrentBlock (LLVMSwitch i1Ty candidateEnd tryMatch [(1, finalSegment)])
      startBlock tryMatch
      emitStore LLVMPtr candidate matchHaystackSlot
      emitStore LLVMPtr delimiter matchDelimiterSlot
      emitStore i64Ty candidateOffset matchHaystackOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) matchDelimiterOffsetSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock matchHeader
      delimiterCursor <- emitAssign "strsplit.match.delimiter" LLVMPtr (LLVMLoad LLVMPtr matchDelimiterSlot)
      delimiterOffset <- emitAssign "strsplit.match.delimiter.offset" i64Ty (LLVMLoad i64Ty matchDelimiterOffsetSlot)
      delimiterDone <- emitAssign "strsplit.match.delimiter.done" i1Ty (LLVMICmpEq delimiterOffset delimiterBytes)
      finishCurrentBlock (LLVMSwitch i1Ty delimiterDone matchHaystackEnd [(1, matchFound)])
      startBlock matchHaystackEnd
      haystackCursor <- emitAssign "strsplit.match.haystack" LLVMPtr (LLVMLoad LLVMPtr matchHaystackSlot)
      haystackOffset <- emitAssign "strsplit.match.haystack.offset" i64Ty (LLVMLoad i64Ty matchHaystackOffsetSlot)
      haystackDone <- emitAssign "strsplit.match.haystack.done" i1Ty (LLVMICmpEq haystackOffset haystackBytes)
      finishCurrentBlock (LLVMSwitch i1Ty haystackDone matchCompare [(1, candidateAdvance)])
      startBlock matchCompare
      delimiterByte <- loadByteAt "strsplit.match.delimiter.byte" delimiterCursor 0
      haystackByte <- loadByteAt "strsplit.match.haystack.byte" haystackCursor 0
      bytesMatch <- emitAssign "strsplit.match.bytes" i1Ty (LLVMICmpEq haystackByte delimiterByte)
      finishCurrentBlock (LLVMSwitch i1Ty bytesMatch candidateAdvance [(1, matchAdvance)])
      startBlock matchAdvance
      nextHaystackCursor <- bytePtrAt "strsplit.match.haystack.next" haystackCursor 1
      nextDelimiterCursor <- bytePtrAt "strsplit.match.delimiter.next" delimiterCursor 1
      nextHaystackOffset <- emitAssign "strsplit.match.haystack.offset.next" i64Ty (LLVMAdd haystackOffset (LLVMIntLiteral 64 1))
      nextDelimiterOffset <- emitAssign "strsplit.match.delimiter.offset.next" i64Ty (LLVMAdd delimiterOffset (LLVMIntLiteral 64 1))
      emitStore LLVMPtr nextHaystackCursor matchHaystackSlot
      emitStore LLVMPtr nextDelimiterCursor matchDelimiterSlot
      emitStore i64Ty nextHaystackOffset matchHaystackOffsetSlot
      emitStore i64Ty nextDelimiterOffset matchDelimiterOffsetSlot
      finishCurrentBlock (LLVMBr matchHeader)
      startBlock matchFound
      matchCandidate <- emitAssign "strsplit.match.candidate" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      afterDelimiter <- bytePtrAtOperand "strsplit.match.after-delimiter" matchCandidate delimiterBytes
      afterDelimiterOffset <- emitAssign "strsplit.match.after-delimiter.offset" i64Ty (LLVMAdd candidateOffset delimiterBytes)
      pushSegment "strsplit.match" matchCandidate afterDelimiter afterDelimiter afterDelimiterOffset
      finishCurrentBlock (LLVMBr candidateHeader)
      startBlock candidateAdvance
      advanceCandidate <- emitAssign "strsplit.advance.candidate" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      advanceByte <- loadByteAt "strsplit.advance.byte" advanceCandidate 0
      asciiClass <- emitAssign "strsplit.advance.ascii.class" i8Ty (LLVMAnd advanceByte (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strsplit.advance.is.ascii" i1Ty (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty isAscii advanceDetectTwo [(1, advanceAscii)])
      startBlock advanceAscii
      finishCandidateAdvance "strsplit.advance.ascii" 1 advanceCandidate
      startBlock advanceDetectTwo
      twoClass <- emitAssign "strsplit.advance.two.class" i8Ty (LLVMAnd advanceByte (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strsplit.advance.is.two" i1Ty (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch i1Ty isTwo advanceDetectThree [(1, advanceTwo)])
      startBlock advanceTwo
      finishCandidateAdvance "strsplit.advance.two" 2 advanceCandidate
      startBlock advanceDetectThree
      threeClass <- emitAssign "strsplit.advance.three.class" i8Ty (LLVMAnd advanceByte (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strsplit.advance.is.three" i1Ty (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch i1Ty isThree advanceFour [(1, advanceThree)])
      startBlock advanceThree
      finishCandidateAdvance "strsplit.advance.three" 3 advanceCandidate
      startBlock advanceFour
      finishCandidateAdvance "strsplit.advance.four" 4 advanceCandidate
      startBlock finalSegment
      finalEnd <- emitAssign "strsplit.final.end" LLVMPtr (LLVMLoad LLVMPtr candidateSlot)
      finalOffset <- emitAssign "strsplit.final.offset" i64Ty (LLVMLoad i64Ty candidateOffsetSlot)
      pushSegment "strsplit.final" finalEnd finalEnd finalEnd finalOffset
      finishCurrentBlock (LLVMBr reverseInit)
      startBlock reverseInit
      revCurSlot <- emitAssign "strsplit.reverse.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      resultSlot <- emitAssign "strsplit.result.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      reversed <- emitAssign "strsplit.reversed" LLVMPtr (LLVMLoad LLVMPtr accSlot)
      emitStore LLVMPtr reversed revCurSlot
      resultNil <- allocateNil "strsplit.result.nil"
      emitStore LLVMPtr resultNil resultSlot
      reverseHeader <- freshBlock "strsplit.reverse.header"
      reverseBody <- freshBlock "strsplit.reverse.body"
      reverseDone <- freshBlock "strsplit.reverse.done"
      finishCurrentBlock (LLVMBr reverseHeader)
      startBlock reverseHeader
      listCell <- emitAssign "strsplit.reverse.cell" LLVMPtr (LLVMLoad LLVMPtr revCurSlot)
      tagPtr <- emitGep "strsplit.reverse.tag.ptr" listCell constructorTagOffset
      tag <- emitAssign "strsplit.reverse.tag" i64Ty (LLVMLoad i64Ty tagPtr)
      isNil <- emitAssign "strsplit.reverse.is.nil" i1Ty (LLVMICmpEq tag (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMSwitch i1Ty isNil reverseBody [(1, reverseDone)])
      startBlock reverseBody
      headPtr <- emitGep "strsplit.reverse.head.ptr" listCell (constructorFieldOffset 0)
      headValue <- emitAssign "strsplit.reverse.head" LLVMPtr (LLVMLoad LLVMPtr headPtr)
      tailPtr <- emitGep "strsplit.reverse.tail.ptr" listCell (constructorFieldOffset 1)
      tailValue <- emitAssign "strsplit.reverse.tail" LLVMPtr (LLVMLoad LLVMPtr tailPtr)
      resultAcc <- emitAssign "strsplit.reverse.acc" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      resultCons <- allocateCons "strsplit.reverse.cons" headValue resultAcc
      emitStore LLVMPtr resultCons resultSlot
      emitStore LLVMPtr tailValue revCurSlot
      finishCurrentBlock (LLVMBr reverseHeader)
      startBlock reverseDone
      result <- emitAssign "strsplit.result" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_split lowering failed: " ++ renderBackendLLVMError err)

nativeStringJoinFunction :: LLVMFunction
nativeStringJoinFunction =
  case
    lowerNativeFunction runtimeStringJoinName LLVMPtr [(LLVMPtr, "separator"), (LLVMPtr, "values")] $ \params -> do
      let separator = requireNativeParam "separator" params
          values = requireNativeParam "values" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          allocateEmpty label = do
            result <- emitAssign (label ++ ".result") LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 1)])
            terminatorPtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty result [(i64Ty, LLVMIntLiteral 64 0)])
            emitStore i8Ty (LLVMIntLiteral 8 0) terminatorPtr
            _ <-
              emitAssign
                (label ++ ".register.length")
                (LLVMInt 32)
                (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, LLVMIntLiteral 64 0)])
            pure result
      resultSlot <- emitAssign "strjoin.result.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      curSlot <- emitAssign "strjoin.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      firstSlot <- emitAssign "strjoin.first.slot" LLVMPtr (LLVMAlloca i1Ty (LLVMIntLiteral 64 1))
      empty <- allocateEmpty "strjoin.empty"
      emitStore LLVMPtr empty resultSlot
      emitStore LLVMPtr values curSlot
      emitStore i1Ty (LLVMIntLiteral 1 1) firstSlot
      header <- freshBlock "strjoin.header"
      body <- freshBlock "strjoin.body"
      firstValue <- freshBlock "strjoin.first"
      laterValue <- freshBlock "strjoin.later"
      done <- freshBlock "strjoin.done"
      invalid <- freshBlock "strjoin.invalid"
      finishCurrentBlock (LLVMBr header)
      startBlock header
      cur <- emitAssign "strjoin.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      tagPtr <- emitGep "strjoin.tag.ptr" cur constructorTagOffset
      tag <- emitAssign "strjoin.tag" i64Ty (LLVMLoad i64Ty tagPtr)
      finishCurrentBlock (LLVMSwitch i64Ty tag invalid [(0, done), (1, body)])
      startBlock body
      headPtr <- emitGep "strjoin.head.ptr" cur (constructorFieldOffset 0)
      headValue <- emitAssign "strjoin.head" LLVMPtr (LLVMLoad LLVMPtr headPtr)
      tailPtr <- emitGep "strjoin.tail.ptr" cur (constructorFieldOffset 1)
      tailValue <- emitAssign "strjoin.tail" LLVMPtr (LLVMLoad LLVMPtr tailPtr)
      isFirst <- emitAssign "strjoin.first.value" i1Ty (LLVMLoad i1Ty firstSlot)
      finishCurrentBlock (LLVMSwitch i1Ty isFirst laterValue [(1, firstValue)])
      startBlock firstValue
      emitStore LLVMPtr headValue resultSlot
      emitStore i1Ty (LLVMIntLiteral 1 0) firstSlot
      emitStore LLVMPtr tailValue curSlot
      finishCurrentBlock (LLVMBr header)
      startBlock laterValue
      current <- emitAssign "strjoin.current" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      withSeparator <-
        emitAssign
          "strjoin.with.separator"
          LLVMPtr
          (LLVMCall runtimeStringAppendName [(LLVMPtr, current), (LLVMPtr, separator)])
      joined <-
        emitAssign
          "strjoin.joined"
          LLVMPtr
          (LLVMCall runtimeStringAppendName [(LLVMPtr, withSeparator), (LLVMPtr, headValue)])
      emitStore LLVMPtr joined resultSlot
      emitStore LLVMPtr tailValue curSlot
      finishCurrentBlock (LLVMBr header)
      startBlock done
      result <- emitAssign "strjoin.result" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      finishCurrentBlock (LLVMRet LLVMPtr result)
      startBlock invalid
      finishCurrentBlock LLVMUnreachable
  of
    Right function -> function
    Left err -> error ("internal native __string_join lowering failed: " ++ renderBackendLLVMError err)

nativeStringSplitCharFunction :: LLVMFunction
nativeStringSplitCharFunction =
  case
    lowerNativeFunction runtimeStringSplitCharName LLVMPtr [(LLVMPtr, "haystack"), (LLVMInt 32, "delimiter")] $ \params -> do
      let haystack = requireNativeParam "haystack" params
          delimiter = requireNativeParam "delimiter" params
          i32Ty = LLVMInt 32
      delimiterString <-
        emitAssign
          "strsplitchar.delimiter"
          LLVMPtr
          (LLVMCall runtimeStringFromCharName [(i32Ty, delimiter)])
      result <-
        emitAssign
          "strsplitchar.result"
          LLVMPtr
          (LLVMCall runtimeStringSplitName [(LLVMPtr, haystack), (LLVMPtr, delimiterString)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_split_char lowering failed: " ++ renderBackendLLVMError err)

nativeStringCompareFunction :: LLVMFunction
nativeStringCompareFunction =
  case
    lowerNativeFunction runtimeStringCompareName (LLVMInt 64) [(LLVMPtr, "left"), (LLVMPtr, "right")] $ \params -> do
      let left = requireNativeParam "left" params
          right = requireNativeParam "right" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          bytePtrAtOperand label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, offset)])
          loadByteAtOperand label base offset = do
            ptr <- bytePtrAtOperand (label ++ ".ptr") base offset
            emitAssign label i8Ty (LLVMLoad i8Ty ptr)
      leftLength <-
        emitAssign
          "strcmp.left.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, left)])
      rightLength <-
        emitAssign
          "strcmp.right.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, right)])
      indexSlot <- emitAssign "strcmp.index.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore i64Ty (LLVMIntLiteral 64 0) indexSlot
      header <- freshBlock "strcmp.header"
      leftDone <- freshBlock "strcmp.left.done"
      rightDone <- freshBlock "strcmp.right.done"
      compareBytes <- freshBlock "strcmp.compare"
      bytesEqual <- freshBlock "strcmp.bytes.equal"
      compareOrder <- freshBlock "strcmp.compare.order"
      leftLess <- freshBlock "strcmp.left.less"
      leftGreater <- freshBlock "strcmp.left.greater"
      equal <- freshBlock "strcmp.equal"
      finishCurrentBlock (LLVMBr header)
      startBlock header
      index <- emitAssign "strcmp.index" i64Ty (LLVMLoad i64Ty indexSlot)
      leftComplete <- emitAssign "strcmp.left.complete" i1Ty (LLVMICmpEq index leftLength)
      finishCurrentBlock (LLVMSwitch i1Ty leftComplete rightDone [(1, leftDone)])
      startBlock leftDone
      rightAlsoComplete <- emitAssign "strcmp.right.also.complete" i1Ty (LLVMICmpEq index rightLength)
      finishCurrentBlock (LLVMSwitch i1Ty rightAlsoComplete leftLess [(1, equal)])
      startBlock rightDone
      rightComplete <- emitAssign "strcmp.right.complete" i1Ty (LLVMICmpEq index rightLength)
      finishCurrentBlock (LLVMSwitch i1Ty rightComplete compareBytes [(1, leftGreater)])
      startBlock compareBytes
      leftByte <- loadByteAtOperand "strcmp.left.byte" left index
      rightByte <- loadByteAtOperand "strcmp.right.byte" right index
      sameByte <- emitAssign "strcmp.same.byte" i1Ty (LLVMICmpEq leftByte rightByte)
      finishCurrentBlock (LLVMSwitch i1Ty sameByte compareOrder [(1, bytesEqual)])
      startBlock bytesEqual
      nextIndex <- emitAssign "strcmp.index.next" i64Ty (LLVMAdd index (LLVMIntLiteral 64 1))
      emitStore i64Ty nextIndex indexSlot
      finishCurrentBlock (LLVMBr header)
      startBlock compareOrder
      rightGreater <- emitAssign "strcmp.right.greater" i1Ty (LLVMICmpUgt rightByte leftByte)
      finishCurrentBlock (LLVMSwitch i1Ty rightGreater leftGreater [(1, leftLess)])
      startBlock leftLess
      finishCurrentBlock (LLVMRet i64Ty (LLVMIntLiteral 64 (-1)))
      startBlock leftGreater
      finishCurrentBlock (LLVMRet i64Ty (LLVMIntLiteral 64 1))
      startBlock equal
      finishCurrentBlock (LLVMRet i64Ty (LLVMIntLiteral 64 0))
  of
    Right function -> function
    Left err -> error ("internal native __string_compare lowering failed: " ++ renderBackendLLVMError err)

nativeStringToAsciiLowerFunction :: LLVMFunction
nativeStringToAsciiLowerFunction =
  nativeStringAsciiCaseFunction
    runtimeStringToAsciiLowerName
    "strtoasciilower"
    64
    91
    (\byte -> LLVMAdd byte (LLVMIntLiteral 8 32))

nativeStringToAsciiUpperFunction :: LLVMFunction
nativeStringToAsciiUpperFunction =
  nativeStringAsciiCaseFunction
    runtimeStringToAsciiUpperName
    "strtoasciiupper"
    96
    123
    (\byte -> LLVMSub byte (LLVMIntLiteral 8 32))

nativeStringAsciiCaseFunction :: String -> String -> Integer -> Integer -> (LLVMOperand -> LLVMExpression) -> LLVMFunction
nativeStringAsciiCaseFunction functionName label lowerBound upperBound transform =
  case
    lowerNativeFunction functionName LLVMPtr [(LLVMPtr, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          bytePtrAtOperand label0 base offset =
            emitAssign label0 LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, offset)])
          loadByteAtOperand label0 base offset = do
            ptr <- bytePtrAtOperand (label0 ++ ".ptr") base offset
            emitAssign label0 i8Ty (LLVMLoad i8Ty ptr)
      byteLength <-
        emitAssign
          (label ++ ".length")
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      allocationSize <- emitAssign (label ++ ".allocation.size") i64Ty (LLVMAdd byteLength (LLVMIntLiteral 64 1))
      result <- emitAssign (label ++ ".result") LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, allocationSize)])
      indexSlot <- emitAssign (label ++ ".index.slot") LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore i64Ty (LLVMIntLiteral 64 0) indexSlot
      header <- freshBlock (label ++ ".header")
      body <- freshBlock (label ++ ".body")
      transformBlock <- freshBlock (label ++ ".transform")
      copyBlock <- freshBlock (label ++ ".copy")
      advance <- freshBlock (label ++ ".advance")
      done <- freshBlock (label ++ ".done")
      finishCurrentBlock (LLVMBr header)
      startBlock header
      index <- emitAssign (label ++ ".index") i64Ty (LLVMLoad i64Ty indexSlot)
      complete <- emitAssign (label ++ ".complete") i1Ty (LLVMICmpEq index byteLength)
      finishCurrentBlock (LLVMSwitch i1Ty complete body [(1, done)])
      startBlock body
      byte <- loadByteAtOperand (label ++ ".byte") value index
      aboveLower <- emitAssign (label ++ ".above.lower") i1Ty (LLVMICmpUgt byte (LLVMIntLiteral 8 lowerBound))
      belowUpper <- emitAssign (label ++ ".below.upper") i1Ty (LLVMICmpUgt (LLVMIntLiteral 8 upperBound) byte)
      shouldTransform <- emitAssign (label ++ ".should.transform") i1Ty (LLVMAnd aboveLower belowUpper)
      finishCurrentBlock (LLVMSwitch i1Ty shouldTransform copyBlock [(1, transformBlock)])
      startBlock transformBlock
      transformed <- emitAssign (label ++ ".transformed") i8Ty (transform byte)
      destPtr <- bytePtrAtOperand (label ++ ".transform.dest.ptr") result index
      emitStore i8Ty transformed destPtr
      finishCurrentBlock (LLVMBr advance)
      startBlock copyBlock
      copyDestPtr <- bytePtrAtOperand (label ++ ".copy.dest.ptr") result index
      emitStore i8Ty byte copyDestPtr
      finishCurrentBlock (LLVMBr advance)
      startBlock advance
      nextIndex <- emitAssign (label ++ ".index.next") i64Ty (LLVMAdd index (LLVMIntLiteral 64 1))
      emitStore i64Ty nextIndex indexSlot
      finishCurrentBlock (LLVMBr header)
      startBlock done
      terminatorPtr <- bytePtrAtOperand (label ++ ".terminator.ptr") result byteLength
      emitStore i8Ty (LLVMIntLiteral 8 0) terminatorPtr
      _ <-
        emitAssign
          (label ++ ".register.length")
          (LLVMInt 32)
          (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, byteLength)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native ASCII string case lowering failed for " ++ functionName ++ ": " ++ renderBackendLLVMError err)

nativeStringFromCharFunction :: LLVMFunction
nativeStringFromCharFunction =
  case
    lowerNativeFunction runtimeStringFromCharName LLVMPtr [(LLVMInt 32, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          bytePtrAt label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
          storeNull label base offset = do
            ptr <- bytePtrAt (label ++ ".ptr") base offset
            emitStore i8Ty (LLVMIntLiteral 8 0) ptr
          finishWith label result packed byteLength = do
            emitStore i32Ty packed result
            storeNull (label ++ ".null") result byteLength
            _ <-
              emitAssign
                (label ++ ".register.length")
                (LLVMInt 32)
                ( LLVMCall
                    nativeStringRegisterLengthFunctionName
                    [(LLVMPtr, result), (i64Ty, LLVMIntLiteral 64 byteLength)]
                )
            finishCurrentBlock (LLVMRet LLVMPtr result)
          shiftByte label byte shift =
            emitAssign label i32Ty (LLVMShl byte (LLVMIntLiteral 32 shift))
          continuationByte label payload =
            emitAssign label i32Ty (LLVMOr (LLVMIntLiteral 32 0x80) payload)
          floorDivBy label divisor operand = do
            quotientSlot <- emitAssign (label ++ ".quotient.slot") LLVMPtr (LLVMAlloca i32Ty (LLVMIntLiteral 64 1))
            thresholdSlot <- emitAssign (label ++ ".threshold.slot") LLVMPtr (LLVMAlloca i32Ty (LLVMIntLiteral 64 1))
            emitStore i32Ty (LLVMIntLiteral 32 0) quotientSlot
            emitStore i32Ty (LLVMIntLiteral 32 divisor) thresholdSlot
            header <- freshBlock (label ++ ".header")
            advance <- freshBlock (label ++ ".advance")
            done <- freshBlock (label ++ ".done")
            finishCurrentBlock (LLVMBr header)
            startBlock header
            threshold <- emitAssign (label ++ ".threshold") i32Ty (LLVMLoad i32Ty thresholdSlot)
            past <- emitAssign (label ++ ".past") (LLVMInt 1) (LLVMICmpUgt threshold operand)
            finishCurrentBlock (LLVMSwitch (LLVMInt 1) past advance [(1, done)])
            startBlock advance
            quotient <- emitAssign (label ++ ".quotient") i32Ty (LLVMLoad i32Ty quotientSlot)
            nextQuotient <- emitAssign (label ++ ".quotient.next") i32Ty (LLVMAdd quotient (LLVMIntLiteral 32 1))
            nextThreshold <- emitAssign (label ++ ".threshold.next") i32Ty (LLVMAdd threshold (LLVMIntLiteral 32 divisor))
            emitStore i32Ty nextQuotient quotientSlot
            emitStore i32Ty nextThreshold thresholdSlot
            finishCurrentBlock (LLVMBr header)
            startBlock done
            emitAssign (label ++ ".result") i32Ty (LLVMLoad i32Ty quotientSlot)
          packBytes label byte0 rest = do
            shifted <- zipWithM (\index byte -> shiftByte (label ++ ".b" ++ show index ++ ".shifted") byte (index * 8)) [1 :: Integer ..] rest
            foldM (\acc byte -> emitAssign (label ++ ".packed") i32Ty (LLVMOr acc byte)) byte0 shifted
      result <- emitAssign "strfromchar.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 5)])
      asciiBlock <- freshBlock "strfromchar.ascii"
      detectTwo <- freshBlock "strfromchar.detect.two"
      twoBlock <- freshBlock "strfromchar.two"
      detectThree <- freshBlock "strfromchar.detect.three"
      threeBlock <- freshBlock "strfromchar.three"
      fourBlock <- freshBlock "strfromchar.four"
      isAscii <- emitAssign "strfromchar.is.ascii" (LLVMInt 1) (LLVMICmpUgt (LLVMIntLiteral 32 128) value)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii detectTwo [(1, asciiBlock)])
      startBlock asciiBlock
      finishWith "strfromchar.ascii" result value 1
      startBlock detectTwo
      isTwo <- emitAssign "strfromchar.is.two" (LLVMInt 1) (LLVMICmpUgt (LLVMIntLiteral 32 2048) value)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo detectThree [(1, twoBlock)])
      startBlock twoBlock
      twoByte0Payload <- floorDivBy "strfromchar.two.b0.payload" 64 value
      twoByte0 <- emitAssign "strfromchar.two.b0" i32Ty (LLVMOr (LLVMIntLiteral 32 0xC0) twoByte0Payload)
      twoByte1Payload <- emitAssign "strfromchar.two.b1.payload" i32Ty (LLVMAnd value (LLVMIntLiteral 32 0x3F))
      twoByte1 <- continuationByte "strfromchar.two.b1" twoByte1Payload
      twoPacked <- packBytes "strfromchar.two" twoByte0 [twoByte1]
      finishWith "strfromchar.two" result twoPacked 2
      startBlock detectThree
      isThree <- emitAssign "strfromchar.is.three" (LLVMInt 1) (LLVMICmpUgt (LLVMIntLiteral 32 65536) value)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree fourBlock [(1, threeBlock)])
      startBlock threeBlock
      threeByte0Payload <- floorDivBy "strfromchar.three.b0.payload" 4096 value
      threeByte0 <- emitAssign "strfromchar.three.b0" i32Ty (LLVMOr (LLVMIntLiteral 32 0xE0) threeByte0Payload)
      threeQ6 <- floorDivBy "strfromchar.three.q6" 64 value
      threeByte1Payload <- emitAssign "strfromchar.three.b1.payload" i32Ty (LLVMAnd threeQ6 (LLVMIntLiteral 32 0x3F))
      threeByte1 <- continuationByte "strfromchar.three.b1" threeByte1Payload
      threeByte2Payload <- emitAssign "strfromchar.three.b2.payload" i32Ty (LLVMAnd value (LLVMIntLiteral 32 0x3F))
      threeByte2 <- continuationByte "strfromchar.three.b2" threeByte2Payload
      threePacked <- packBytes "strfromchar.three" threeByte0 [threeByte1, threeByte2]
      finishWith "strfromchar.three" result threePacked 3
      startBlock fourBlock
      fourByte0Payload <- floorDivBy "strfromchar.four.b0.payload" 262144 value
      fourByte0 <- emitAssign "strfromchar.four.b0" i32Ty (LLVMOr (LLVMIntLiteral 32 0xF0) fourByte0Payload)
      fourQ12 <- floorDivBy "strfromchar.four.q12" 4096 value
      fourByte1Payload <- emitAssign "strfromchar.four.b1.payload" i32Ty (LLVMAnd fourQ12 (LLVMIntLiteral 32 0x3F))
      fourByte1 <- continuationByte "strfromchar.four.b1" fourByte1Payload
      fourQ6 <- floorDivBy "strfromchar.four.q6" 64 value
      fourByte2Payload <- emitAssign "strfromchar.four.b2.payload" i32Ty (LLVMAnd fourQ6 (LLVMIntLiteral 32 0x3F))
      fourByte2 <- continuationByte "strfromchar.four.b2" fourByte2Payload
      fourByte3Payload <- emitAssign "strfromchar.four.b3.payload" i32Ty (LLVMAnd value (LLVMIntLiteral 32 0x3F))
      fourByte3 <- continuationByte "strfromchar.four.b3" fourByte3Payload
      fourPacked <- packBytes "strfromchar.four" fourByte0 [fourByte1, fourByte2, fourByte3]
      finishWith "strfromchar.four" result fourPacked 4
  of
    Right function -> function
    Left err -> error ("internal native __string_from_char lowering failed: " ++ renderBackendLLVMError err)

nativeStringFromIntFunction :: LLVMFunction
nativeStringFromIntFunction =
  case
    lowerNativeFunction runtimeStringFromIntName LLVMPtr [(LLVMInt 64, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
      result <- emitAssign "strfromint.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 22)])
      _ <-
        emitAssign
          "strfromint.render"
          i32Ty
          ( LLVMCallVarArgs
              nativeSprintfName
              [LLVMPtr, LLVMPtr]
              [ (LLVMPtr, result),
                (LLVMPtr, LLVMGlobalRef LLVMPtr nativeFmtIntName),
                (i64Ty, value)
              ]
          )
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_from_int lowering failed: " ++ renderBackendLLVMError err)

nativeStringFromBoolFunction :: LLVMFunction
nativeStringFromBoolFunction =
  case
    lowerNativeFunction runtimeStringFromBoolName LLVMPtr [(LLVMInt 1, "value")] $ \params -> do
      let value = requireNativeParam "value" params
      falseBlock <- freshBlock "strfrombool.false"
      trueBlock <- freshBlock "strfrombool.true"
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) value falseBlock [(1, trueBlock)])
      startBlock trueBlock
      finishCurrentBlock (LLVMRet LLVMPtr (LLVMGlobalRef LLVMPtr nativeStrTrueName))
      startBlock falseBlock
      finishCurrentBlock (LLVMRet LLVMPtr (LLVMGlobalRef LLVMPtr nativeStrFalseName))
  of
    Right function -> function
    Left err -> error ("internal native __string_from_bool lowering failed: " ++ renderBackendLLVMError err)

nativeStringFromNatFunction :: LLVMFunction
nativeStringFromNatFunction =
  case
    lowerNativeFunction runtimeStringFromNatName LLVMPtr [(LLVMPtr, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i64Ty = LLVMInt 64
      countSlot <- emitAssign "strfromnat.count.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      curSlot <- emitAssign "strfromnat.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore i64Ty (LLVMIntLiteral 64 0) countSlot
      emitStore LLVMPtr value curSlot
      scanHeader <- freshBlock "strfromnat.scan.header"
      scanSucc <- freshBlock "strfromnat.scan.succ"
      scanDone <- freshBlock "strfromnat.scan.done"
      scanInvalid <- freshBlock "strfromnat.scan.invalid"
      finishCurrentBlock (LLVMBr scanHeader)
      startBlock scanHeader
      cur <- emitAssign "strfromnat.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      tagPtr <- emitGep "strfromnat.tag.ptr" cur constructorTagOffset
      tag <- emitAssign "strfromnat.tag" i64Ty (LLVMLoad i64Ty tagPtr)
      finishCurrentBlock (LLVMSwitch i64Ty tag scanInvalid [(0, scanDone), (1, scanSucc)])
      startBlock scanSucc
      nextPtr <- emitGep "strfromnat.succ.next.ptr" cur (constructorFieldOffset 0)
      next <- emitAssign "strfromnat.succ.next" LLVMPtr (LLVMLoad LLVMPtr nextPtr)
      count <- emitAssign "strfromnat.count" i64Ty (LLVMLoad i64Ty countSlot)
      nextCount <- emitAssign "strfromnat.next.count" i64Ty (LLVMAdd count (LLVMIntLiteral 64 1))
      emitStore i64Ty nextCount countSlot
      emitStore LLVMPtr next curSlot
      finishCurrentBlock (LLVMBr scanHeader)
      startBlock scanInvalid
      finishCurrentBlock LLVMUnreachable
      startBlock scanDone
      finalCount <- emitAssign "strfromnat.final.count" i64Ty (LLVMLoad i64Ty countSlot)
      result <- emitAssign "strfromnat.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 22)])
      _ <-
        emitAssign
          "strfromnat.render"
          (LLVMInt 32)
          ( LLVMCallVarArgs
              nativeSprintfName
              [LLVMPtr, LLVMPtr]
              [ (LLVMPtr, result),
                (LLVMPtr, LLVMGlobalRef LLVMPtr nativeFmtIntName),
                (i64Ty, finalCount)
              ]
          )
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_from_nat lowering failed: " ++ renderBackendLLVMError err)

nativeStringFromListFunction :: LLVMFunction
nativeStringFromListFunction =
  case
    lowerNativeFunction runtimeStringFromListName LLVMPtr [(LLVMPtr, "values")] $ \params -> do
      let values = requireNativeParam "values" params
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          allocateEmpty label = do
            result <- emitAssign (label ++ ".result") LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 1)])
            terminatorPtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty result [(i64Ty, LLVMIntLiteral 64 0)])
            emitStore i8Ty (LLVMIntLiteral 8 0) terminatorPtr
            _ <-
              emitAssign
                (label ++ ".register.length")
                i32Ty
                (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, LLVMIntLiteral 64 0)])
            pure result
      resultSlot <- emitAssign "strfromlist.result.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      curSlot <- emitAssign "strfromlist.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      empty <- allocateEmpty "strfromlist.empty"
      emitStore LLVMPtr empty resultSlot
      emitStore LLVMPtr values curSlot
      header <- freshBlock "strfromlist.header"
      body <- freshBlock "strfromlist.body"
      done <- freshBlock "strfromlist.done"
      invalid <- freshBlock "strfromlist.invalid"
      finishCurrentBlock (LLVMBr header)
      startBlock header
      cur <- emitAssign "strfromlist.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      tagPtr <- emitGep "strfromlist.tag.ptr" cur constructorTagOffset
      tag <- emitAssign "strfromlist.tag" i64Ty (LLVMLoad i64Ty tagPtr)
      finishCurrentBlock (LLVMSwitch i64Ty tag invalid [(0, done), (1, body)])
      startBlock body
      headPtr <- emitGep "strfromlist.head.ptr" cur (constructorFieldOffset 0)
      headValue <- emitAssign "strfromlist.head" i32Ty (LLVMLoad i32Ty headPtr)
      tailPtr <- emitGep "strfromlist.tail.ptr" cur (constructorFieldOffset 1)
      tailValue <- emitAssign "strfromlist.tail" LLVMPtr (LLVMLoad LLVMPtr tailPtr)
      headString <-
        emitAssign
          "strfromlist.head.string"
          LLVMPtr
          (LLVMCall runtimeStringFromCharName [(i32Ty, headValue)])
      current <- emitAssign "strfromlist.current" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      appended <-
        emitAssign
          "strfromlist.appended"
          LLVMPtr
          (LLVMCall runtimeStringAppendName [(LLVMPtr, current), (LLVMPtr, headString)])
      emitStore LLVMPtr appended resultSlot
      emitStore LLVMPtr tailValue curSlot
      finishCurrentBlock (LLVMBr header)
      startBlock done
      result <- emitAssign "strfromlist.result" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      finishCurrentBlock (LLVMRet LLVMPtr result)
      startBlock invalid
      finishCurrentBlock LLVMUnreachable
  of
    Right function -> function
    Left err -> error ("internal native __string_from_list lowering failed: " ++ renderBackendLLVMError err)

nativeStringToListFunction :: LLVMFunction
nativeStringToListFunction =
  case
    lowerNativeFunction runtimeStringToListName LLVMPtr [(LLVMPtr, "value")] $ \params -> do
      let value = requireNativeParam "value" params
          i1Ty = LLVMInt 1
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          bytePtrAt label base offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty base [(i64Ty, LLVMIntLiteral 64 offset)])
          loadByteAt label base offset = do
            ptr <- bytePtrAt (label ++ ".ptr") base offset
            emitAssign label i8Ty (LLVMLoad i8Ty ptr)
          advancePtr label base offset =
            bytePtrAt label base offset
          zextByte label byte =
            emitAssign label i32Ty (LLVMZext byte i32Ty)
          shiftedPayload label byte mask shift = do
            masked <- emitAssign (label ++ ".masked") i8Ty (LLVMAnd byte (LLVMIntLiteral 8 mask))
            extended <- zextByte (label ++ ".i32") masked
            if shift == 0
              then pure extended
              else emitAssign (label ++ ".shifted") i32Ty (LLVMShl extended (LLVMIntLiteral 32 shift))
          allocateNil label = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 0)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 0) tagPtr
            pure cell
          allocateCons label char tailValue = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 2)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 1) tagPtr
            headPtr <- emitGep (label ++ ".head") cell (constructorFieldOffset 0)
            emitStore i32Ty char headPtr
            tailPtr <- emitGep (label ++ ".tail") cell (constructorFieldOffset 1)
            emitStore LLVMPtr tailValue tailPtr
            pure cell
          pushAndAdvance accSlot curSlot offsetSlot scanHeader label scalar nextPtr byteCount = do
            acc <- emitAssign (label ++ ".acc") LLVMPtr (LLVMLoad LLVMPtr accSlot)
            cons <- allocateCons (label ++ ".cons") scalar acc
            offset <- emitAssign (label ++ ".offset") i64Ty (LLVMLoad i64Ty offsetSlot)
            nextOffset <- emitAssign (label ++ ".offset.next") i64Ty (LLVMAdd offset (LLVMIntLiteral 64 byteCount))
            emitStore LLVMPtr cons accSlot
            emitStore LLVMPtr nextPtr curSlot
            emitStore i64Ty nextOffset offsetSlot
            finishCurrentBlock (LLVMBr scanHeader)
      curSlot <- emitAssign "strtolist.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      offsetSlot <- emitAssign "strtolist.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      accSlot <- emitAssign "strtolist.acc.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      byteLength <-
        emitAssign
          "strtolist.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      emitStore LLVMPtr value curSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) offsetSlot
      initialNil <- allocateNil "strtolist.initial.nil"
      emitStore LLVMPtr initialNil accSlot
      scanHeader <- freshBlock "strtolist.scan.header"
      scanDecode <- freshBlock "strtolist.scan.decode"
      scanAscii <- freshBlock "strtolist.scan.ascii"
      scanDetectTwo <- freshBlock "strtolist.scan.detect.two"
      scanTwo <- freshBlock "strtolist.scan.two"
      scanDetectThree <- freshBlock "strtolist.scan.detect.three"
      scanThree <- freshBlock "strtolist.scan.three"
      scanFour <- freshBlock "strtolist.scan.four"
      reverseInit <- freshBlock "strtolist.reverse.init"
      finishCurrentBlock (LLVMBr scanHeader)
      startBlock scanHeader
      cur <- emitAssign "strtolist.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      offset <- emitAssign "strtolist.offset" i64Ty (LLVMLoad i64Ty offsetSlot)
      doneScanning <- emitAssign "strtolist.done.scanning" i1Ty (LLVMICmpEq offset byteLength)
      finishCurrentBlock (LLVMSwitch i1Ty doneScanning scanDecode [(1, reverseInit)])
      startBlock scanDecode
      byte0 <- loadByteAt "strtolist.byte0" cur 0
      asciiClass <- emitAssign "strtolist.ascii.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strtolist.is.ascii" i1Ty (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch i1Ty isAscii scanDetectTwo [(1, scanAscii)])
      startBlock scanAscii
      asciiScalar <- zextByte "strtolist.ascii.scalar" byte0
      asciiNext <- advancePtr "strtolist.ascii.next" cur 1
      pushAndAdvance accSlot curSlot offsetSlot scanHeader "strtolist.ascii" asciiScalar asciiNext 1
      startBlock scanDetectTwo
      twoClass <- emitAssign "strtolist.two.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strtolist.is.two" i1Ty (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch i1Ty isTwo scanDetectThree [(1, scanTwo)])
      startBlock scanTwo
      twoByte1 <- loadByteAt "strtolist.two.b1" cur 1
      twoByte0Shifted <- shiftedPayload "strtolist.two.b0" byte0 0x1F 6
      twoByte1Value <- shiftedPayload "strtolist.two.b1.payload" twoByte1 0x3F 0
      twoScalar <- emitAssign "strtolist.two.scalar" i32Ty (LLVMOr twoByte0Shifted twoByte1Value)
      twoNext <- advancePtr "strtolist.two.next" cur 2
      pushAndAdvance accSlot curSlot offsetSlot scanHeader "strtolist.two" twoScalar twoNext 2
      startBlock scanDetectThree
      threeClass <- emitAssign "strtolist.three.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strtolist.is.three" i1Ty (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch i1Ty isThree scanFour [(1, scanThree)])
      startBlock scanThree
      threeByte1 <- loadByteAt "strtolist.three.b1" cur 1
      threeByte2 <- loadByteAt "strtolist.three.b2" cur 2
      threeByte0Shifted <- shiftedPayload "strtolist.three.b0" byte0 0x0F 12
      threeByte1Shifted <- shiftedPayload "strtolist.three.b1.payload" threeByte1 0x3F 6
      threePrefix <- emitAssign "strtolist.three.prefix" i32Ty (LLVMOr threeByte0Shifted threeByte1Shifted)
      threeByte2Value <- shiftedPayload "strtolist.three.b2.payload" threeByte2 0x3F 0
      threeScalar <- emitAssign "strtolist.three.scalar" i32Ty (LLVMOr threePrefix threeByte2Value)
      threeNext <- advancePtr "strtolist.three.next" cur 3
      pushAndAdvance accSlot curSlot offsetSlot scanHeader "strtolist.three" threeScalar threeNext 3
      startBlock scanFour
      fourByte1 <- loadByteAt "strtolist.four.b1" cur 1
      fourByte2 <- loadByteAt "strtolist.four.b2" cur 2
      fourByte3 <- loadByteAt "strtolist.four.b3" cur 3
      fourByte0Shifted <- shiftedPayload "strtolist.four.b0" byte0 0x07 18
      fourByte1Shifted <- shiftedPayload "strtolist.four.b1.payload" fourByte1 0x3F 12
      fourPrefix0 <- emitAssign "strtolist.four.prefix0" i32Ty (LLVMOr fourByte0Shifted fourByte1Shifted)
      fourByte2Shifted <- shiftedPayload "strtolist.four.b2.payload" fourByte2 0x3F 6
      fourPrefix1 <- emitAssign "strtolist.four.prefix1" i32Ty (LLVMOr fourPrefix0 fourByte2Shifted)
      fourByte3Value <- shiftedPayload "strtolist.four.b3.payload" fourByte3 0x3F 0
      fourScalar <- emitAssign "strtolist.four.scalar" i32Ty (LLVMOr fourPrefix1 fourByte3Value)
      fourNext <- advancePtr "strtolist.four.next" cur 4
      pushAndAdvance accSlot curSlot offsetSlot scanHeader "strtolist.four" fourScalar fourNext 4
      startBlock reverseInit
      revCurSlot <- emitAssign "strtolist.reverse.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      resultSlot <- emitAssign "strtolist.result.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      reversed <- emitAssign "strtolist.reversed" LLVMPtr (LLVMLoad LLVMPtr accSlot)
      emitStore LLVMPtr reversed revCurSlot
      resultNil <- allocateNil "strtolist.result.nil"
      emitStore LLVMPtr resultNil resultSlot
      reverseHeader <- freshBlock "strtolist.reverse.header"
      reverseBody <- freshBlock "strtolist.reverse.body"
      reverseDone <- freshBlock "strtolist.reverse.done"
      finishCurrentBlock (LLVMBr reverseHeader)
      startBlock reverseHeader
      listCell <- emitAssign "strtolist.reverse.cell" LLVMPtr (LLVMLoad LLVMPtr revCurSlot)
      tagPtr <- emitGep "strtolist.reverse.tag.ptr" listCell constructorTagOffset
      tag <- emitAssign "strtolist.reverse.tag" i64Ty (LLVMLoad i64Ty tagPtr)
      isNil <- emitAssign "strtolist.reverse.is.nil" i1Ty (LLVMICmpEq tag (LLVMIntLiteral 64 0))
      finishCurrentBlock (LLVMSwitch i1Ty isNil reverseBody [(1, reverseDone)])
      startBlock reverseBody
      headPtr <- emitGep "strtolist.reverse.head.ptr" listCell (constructorFieldOffset 0)
      headValue <- emitAssign "strtolist.reverse.head" i32Ty (LLVMLoad i32Ty headPtr)
      tailPtr <- emitGep "strtolist.reverse.tail.ptr" listCell (constructorFieldOffset 1)
      tailValue <- emitAssign "strtolist.reverse.tail" LLVMPtr (LLVMLoad LLVMPtr tailPtr)
      resultAcc <- emitAssign "strtolist.reverse.acc" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      resultCons <- allocateCons "strtolist.reverse.cons" headValue resultAcc
      emitStore LLVMPtr resultCons resultSlot
      emitStore LLVMPtr tailValue revCurSlot
      finishCurrentBlock (LLVMBr reverseHeader)
      startBlock reverseDone
      result <- emitAssign "strtolist.result" LLVMPtr (LLVMLoad LLVMPtr resultSlot)
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_to_list lowering failed: " ++ renderBackendLLVMError err)

nativeStringDropFunction :: LLVMFunction
nativeStringDropFunction =
  case
    lowerNativeFunction runtimeStringDropName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 64, "count")] $ \params -> do
      let value = requireNativeParam "value" params
          count = requireNativeParam "count" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte label curPtr = do
            bytePtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          advancePtr label offset curPtr =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
      curSlot <- emitAssign "strdrop.cur.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      droppedSlot <- emitAssign "strdrop.dropped.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      byteOffsetSlot <- emitAssign "strdrop.byte.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      byteLength <-
        emitAssign
          "strdrop.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      let advanceScalar label offset curPtr = do
            nextPtr <- advancePtr (label ++ ".next.ptr") offset curPtr
            dropped <- emitAssign (label ++ ".dropped") i64Ty (LLVMLoad i64Ty droppedSlot)
            nextDropped <- emitAssign (label ++ ".dropped.next") i64Ty (LLVMAdd dropped (LLVMIntLiteral 64 1))
            byteOffset <- emitAssign (label ++ ".byte.offset") i64Ty (LLVMLoad i64Ty byteOffsetSlot)
            nextByteOffset <- emitAssign (label ++ ".byte.offset.next") i64Ty (LLVMAdd byteOffset (LLVMIntLiteral 64 offset))
            emitStore i64Ty nextDropped droppedSlot
            emitStore i64Ty nextByteOffset byteOffsetSlot
            emitStore LLVMPtr nextPtr curSlot
      emitStore LLVMPtr value curSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) droppedSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) byteOffsetSlot
      loopHeader <- freshBlock "strdrop.header"
      readByte <- freshBlock "strdrop.read-byte"
      detectAscii <- freshBlock "strdrop.detect.ascii"
      advanceOne <- freshBlock "strdrop.advance.one"
      detectTwo <- freshBlock "strdrop.detect.two"
      advanceTwo <- freshBlock "strdrop.advance.two"
      detectThree <- freshBlock "strdrop.detect.three"
      advanceThree <- freshBlock "strdrop.advance.three"
      advanceFour <- freshBlock "strdrop.advance.four"
      done <- freshBlock "strdrop.done"
      countZero <- emitAssign "strdrop.count.zero" (LLVMInt 1) (LLVMICmpEq count (LLVMIntLiteral 64 0))
      countNegative <- emitAssign "strdrop.count.negative" (LLVMInt 1) (LLVMICmpUgt count (LLVMIntLiteral 64 9223372036854775807))
      countNonPositive <- emitAssign "strdrop.count.nonpositive" (LLVMInt 1) (LLVMOr countZero countNegative)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) countNonPositive loopHeader [(1, done)])
      startBlock loopHeader
      dropped <- emitAssign "strdrop.dropped" i64Ty (LLVMLoad i64Ty droppedSlot)
      droppedEnough <- emitAssign "strdrop.dropped.enough" (LLVMInt 1) (LLVMICmpEq dropped count)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) droppedEnough readByte [(1, done)])
      startBlock readByte
      curPtr <- emitAssign "strdrop.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      byteOffset <- emitAssign "strdrop.byte.offset" i64Ty (LLVMLoad i64Ty byteOffsetSlot)
      byteOffsetDone <- emitAssign "strdrop.byte.offset.done" (LLVMInt 1) (LLVMICmpEq byteOffset byteLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) byteOffsetDone detectAscii [(1, done)])
      startBlock detectAscii
      byte <- loadByte "strdrop.byte" curPtr
      asciiClass <- emitAssign "strdrop.ascii.class" i8Ty (LLVMAnd byte (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strdrop.is.ascii" (LLVMInt 1) (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii detectTwo [(1, advanceOne)])
      startBlock advanceOne
      advanceScalar "strdrop.one" 1 curPtr
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock detectTwo
      twoClass <- emitAssign "strdrop.two.class" i8Ty (LLVMAnd byte (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strdrop.is.two" (LLVMInt 1) (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo detectThree [(1, advanceTwo)])
      startBlock advanceTwo
      advanceScalar "strdrop.two" 2 curPtr
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock detectThree
      threeClass <- emitAssign "strdrop.three.class" i8Ty (LLVMAnd byte (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strdrop.is.three" (LLVMInt 1) (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree advanceFour [(1, advanceThree)])
      startBlock advanceThree
      advanceScalar "strdrop.three" 3 curPtr
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock advanceFour
      advanceScalar "strdrop.four" 4 curPtr
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock done
      result <- emitAssign "strdrop.result" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      finalOffset <- emitAssign "strdrop.final.offset" i64Ty (LLVMLoad i64Ty byteOffsetSlot)
      remainingBytes <- emitAssign "strdrop.remaining.bytes" i64Ty (LLVMSub byteLength finalOffset)
      _ <-
        emitAssign
          "strdrop.register.length"
          (LLVMInt 32)
          (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, remainingBytes)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_drop lowering failed: " ++ renderBackendLLVMError err)

nativeStringTakeFunction :: LLVMFunction
nativeStringTakeFunction =
  case
    lowerNativeFunction runtimeStringTakeName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 64, "count")] $ \params -> do
      let value = requireNativeParam "value" params
          count = requireNativeParam "count" params
          i8Ty = LLVMInt 8
          i64Ty = LLVMInt 64
          loadByte label curPtr offset = do
            bytePtr <- emitAssign (label ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
            emitAssign label i8Ty (LLVMLoad i8Ty bytePtr)
          ptrAt label curPtr offset =
            emitAssign label LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
          copyByte label source dest offset = do
            byte <- loadByte (label ++ ".byte") source offset
            destPtr <- ptrAt (label ++ ".dest") dest offset
            emitStore i8Ty byte destPtr
          advanceScalar label byteCount source dest takenSlot sourceOffsetSlot resultByteLengthSlot sourceSlot destSlot = do
            mapM_ (copyByte label source dest) [0 .. byteCount - 1]
            nextSource <- ptrAt (label ++ ".source.next") source byteCount
            nextDest <- ptrAt (label ++ ".dest.next") dest byteCount
            taken <- emitAssign (label ++ ".taken") i64Ty (LLVMLoad i64Ty takenSlot)
            nextTaken <- emitAssign (label ++ ".taken.next") i64Ty (LLVMAdd taken (LLVMIntLiteral 64 1))
            sourceOffset <- emitAssign (label ++ ".source.offset") i64Ty (LLVMLoad i64Ty sourceOffsetSlot)
            nextSourceOffset <- emitAssign (label ++ ".source.offset.next") i64Ty (LLVMAdd sourceOffset (LLVMIntLiteral 64 byteCount))
            resultByteLength <- emitAssign (label ++ ".result.byte.length") i64Ty (LLVMLoad i64Ty resultByteLengthSlot)
            nextResultByteLength <- emitAssign (label ++ ".result.byte.length.next") i64Ty (LLVMAdd resultByteLength (LLVMIntLiteral 64 byteCount))
            emitStore i64Ty nextTaken takenSlot
            emitStore i64Ty nextSourceOffset sourceOffsetSlot
            emitStore i64Ty nextResultByteLength resultByteLengthSlot
            emitStore LLVMPtr nextSource sourceSlot
            emitStore LLVMPtr nextDest destSlot
      countZero <- emitAssign "strtake.count.zero" (LLVMInt 1) (LLVMICmpEq count (LLVMIntLiteral 64 0))
      countNegative <- emitAssign "strtake.count.negative" (LLVMInt 1) (LLVMICmpUgt count (LLVMIntLiteral 64 9223372036854775807))
      countNonPositive <- emitAssign "strtake.count.nonpositive" (LLVMInt 1) (LLVMOr countZero countNegative)
      allocatePositive <- freshBlock "strtake.allocate.positive"
      allocateEmpty <- freshBlock "strtake.allocate.empty"
      loopHeader <- freshBlock "strtake.header"
      readByte <- freshBlock "strtake.read-byte"
      detectAscii <- freshBlock "strtake.detect.ascii"
      copyOne <- freshBlock "strtake.copy.one"
      detectTwo <- freshBlock "strtake.detect.two"
      copyTwo <- freshBlock "strtake.copy.two"
      detectThree <- freshBlock "strtake.detect.three"
      copyThree <- freshBlock "strtake.copy.three"
      copyFour <- freshBlock "strtake.copy.four"
      done <- freshBlock "strtake.done"
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) countNonPositive allocatePositive [(1, allocateEmpty)])
      startBlock allocateEmpty
      emptyResult <- emitAssign "strtake.empty.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 1)])
      emptyBytePtr <- ptrAt "strtake.empty.ptr" emptyResult 0
      emitStore i8Ty (LLVMIntLiteral 8 0) emptyBytePtr
      _ <-
        emitAssign
          "strtake.empty.register.length"
          (LLVMInt 32)
          (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, emptyResult), (i64Ty, LLVMIntLiteral 64 0)])
      finishCurrentBlock (LLVMRet LLVMPtr emptyResult)
      startBlock allocatePositive
      sourceByteLength <-
        emitAssign
          "strtake.source.byte.length"
          i64Ty
          (LLVMCall nativeStringByteLengthFunctionName [(LLVMPtr, value)])
      doubledCount <- emitAssign "strtake.count.double" i64Ty (LLVMAdd count count)
      maxBytes <- emitAssign "strtake.count.max-bytes" i64Ty (LLVMAdd doubledCount doubledCount)
      allocationSize <- emitAssign "strtake.count.alloc-size" i64Ty (LLVMAdd maxBytes (LLVMIntLiteral 64 1))
      result <- emitAssign "strtake.result" LLVMPtr (LLVMCall runtimeMallocName [(i64Ty, allocationSize)])
      sourceSlot <- emitAssign "strtake.source.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      destSlot <- emitAssign "strtake.dest.slot" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      takenSlot <- emitAssign "strtake.taken.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      sourceOffsetSlot <- emitAssign "strtake.source.offset.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      resultByteLengthSlot <- emitAssign "strtake.result.byte.length.slot" LLVMPtr (LLVMAlloca i64Ty (LLVMIntLiteral 64 1))
      emitStore LLVMPtr value sourceSlot
      emitStore LLVMPtr result destSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) takenSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) sourceOffsetSlot
      emitStore i64Ty (LLVMIntLiteral 64 0) resultByteLengthSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      taken <- emitAssign "strtake.taken" i64Ty (LLVMLoad i64Ty takenSlot)
      takenEnough <- emitAssign "strtake.taken.enough" (LLVMInt 1) (LLVMICmpEq taken count)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) takenEnough readByte [(1, done)])
      startBlock readByte
      source <- emitAssign "strtake.source" LLVMPtr (LLVMLoad LLVMPtr sourceSlot)
      dest <- emitAssign "strtake.dest" LLVMPtr (LLVMLoad LLVMPtr destSlot)
      sourceOffset <- emitAssign "strtake.source.offset" i64Ty (LLVMLoad i64Ty sourceOffsetSlot)
      sourceComplete <- emitAssign "strtake.source.complete" (LLVMInt 1) (LLVMICmpEq sourceOffset sourceByteLength)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) sourceComplete detectAscii [(1, done)])
      startBlock detectAscii
      byte <- loadByte "strtake.byte" source 0
      asciiClass <- emitAssign "strtake.ascii.class" i8Ty (LLVMAnd byte (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strtake.is.ascii" (LLVMInt 1) (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii detectTwo [(1, copyOne)])
      startBlock copyOne
      advanceScalar "strtake.one" 1 source dest takenSlot sourceOffsetSlot resultByteLengthSlot sourceSlot destSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock detectTwo
      twoClass <- emitAssign "strtake.two.class" i8Ty (LLVMAnd byte (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strtake.is.two" (LLVMInt 1) (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo detectThree [(1, copyTwo)])
      startBlock copyTwo
      advanceScalar "strtake.two" 2 source dest takenSlot sourceOffsetSlot resultByteLengthSlot sourceSlot destSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock detectThree
      threeClass <- emitAssign "strtake.three.class" i8Ty (LLVMAnd byte (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strtake.is.three" (LLVMInt 1) (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree copyFour [(1, copyThree)])
      startBlock copyThree
      advanceScalar "strtake.three" 3 source dest takenSlot sourceOffsetSlot resultByteLengthSlot sourceSlot destSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock copyFour
      advanceScalar "strtake.four" 4 source dest takenSlot sourceOffsetSlot resultByteLengthSlot sourceSlot destSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock done
      destDone <- emitAssign "strtake.done.dest" LLVMPtr (LLVMLoad LLVMPtr destSlot)
      terminatorPtr <- ptrAt "strtake.done.ptr" destDone 0
      emitStore i8Ty (LLVMIntLiteral 8 0) terminatorPtr
      resultByteLength <- emitAssign "strtake.result.byte.length" i64Ty (LLVMLoad i64Ty resultByteLengthSlot)
      _ <-
        emitAssign
          "strtake.register.length"
          (LLVMInt 32)
          (LLVMCall nativeStringRegisterLengthFunctionName [(LLVMPtr, result), (i64Ty, resultByteLength)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
  of
    Right function -> function
    Left err -> error ("internal native __string_take lowering failed: " ++ renderBackendLLVMError err)

nativeStringSliceFunction :: LLVMFunction
nativeStringSliceFunction =
  case
    lowerNativeFunction runtimeStringSliceName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 64, "start"), (LLVMInt 64, "count")] $ \params -> do
      let value = requireNativeParam "value" params
          start = requireNativeParam "start" params
          count = requireNativeParam "count" params
      dropped <-
        emitAssign
          "strslice.drop"
          LLVMPtr
          ( LLVMCall
              runtimeStringDropName
              [(LLVMPtr, value), (LLVMInt 64, start)]
          )
      sliced <-
        emitAssign
          "strslice.take"
          LLVMPtr
          ( LLVMCall
              runtimeStringTakeName
              [(LLVMPtr, dropped), (LLVMInt 64, count)]
          )
      finishCurrentBlock (LLVMRet LLVMPtr sliced)
  of
    Right function -> function
    Left err -> error ("internal native __string_slice lowering failed: " ++ renderBackendLLVMError err)

nativeStringCharAtFunction :: LLVMFunction
nativeStringCharAtFunction =
  case
    lowerNativeFunction runtimeStringCharAtName (LLVMInt 32) [(LLVMPtr, "value"), (LLVMInt 64, "index")] $ \params -> do
      let value = requireNativeParam "value" params
          index = requireNativeParam "index" params
          i8Ty = LLVMInt 8
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          loadByte prefix curPtr offset = do
            bytePtr <- emitAssign (prefix ++ ".ptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 offset)])
            emitAssign prefix i8Ty (LLVMLoad i8Ty bytePtr)
          extendByte prefix byte =
            emitAssign prefix i32Ty (LLVMZext byte i32Ty)
      cursor <-
        emitAssign
          "strcharat.cursor"
          LLVMPtr
          ( LLVMCall
              runtimeStringDropName
              [(LLVMPtr, value), (LLVMInt 64, index)]
          )
      byte0 <- loadByte "strcharat.b0" cursor 0
      asciiScalar <- freshBlock "strcharat.ascii"
      detectTwo <- freshBlock "strcharat.detect.two"
      twoByteScalar <- freshBlock "strcharat.two"
      detectThree <- freshBlock "strcharat.detect.three"
      threeByteScalar <- freshBlock "strcharat.three"
      fourByteScalar <- freshBlock "strcharat.four"
      asciiClass <- emitAssign "strcharat.ascii.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x80))
      isAscii <- emitAssign "strcharat.is.ascii" (LLVMInt 1) (LLVMICmpEq asciiClass (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isAscii detectTwo [(1, asciiScalar)])
      startBlock asciiScalar
      asciiValue <- extendByte "strcharat.ascii.value" byte0
      finishCurrentBlock (LLVMRet i32Ty asciiValue)
      startBlock detectTwo
      twoClass <- emitAssign "strcharat.two.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xE0))
      isTwo <- emitAssign "strcharat.is.two" (LLVMInt 1) (LLVMICmpEq twoClass (LLVMIntLiteral 8 0xC0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isTwo detectThree [(1, twoByteScalar)])
      startBlock twoByteScalar
      twoByte0Masked <- emitAssign "strcharat.two.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x1F))
      twoByte0Value <- extendByte "strcharat.two.b0.value" twoByte0Masked
      twoByte0Shifted <- emitAssign "strcharat.two.b0.shifted" i32Ty (LLVMShl twoByte0Value (LLVMIntLiteral 32 6))
      twoByte1 <- loadByte "strcharat.two.b1" cursor 1
      twoByte1Masked <- emitAssign "strcharat.two.b1.masked" i8Ty (LLVMAnd twoByte1 (LLVMIntLiteral 8 0x3F))
      twoByte1Value <- extendByte "strcharat.two.b1.value" twoByte1Masked
      twoScalar <- emitAssign "strcharat.two.scalar" i32Ty (LLVMOr twoByte0Shifted twoByte1Value)
      finishCurrentBlock (LLVMRet i32Ty twoScalar)
      startBlock detectThree
      threeClass <- emitAssign "strcharat.three.class" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0xF0))
      isThree <- emitAssign "strcharat.is.three" (LLVMInt 1) (LLVMICmpEq threeClass (LLVMIntLiteral 8 0xE0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isThree fourByteScalar [(1, threeByteScalar)])
      startBlock threeByteScalar
      threeByte0Masked <- emitAssign "strcharat.three.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x0F))
      threeByte0Value <- extendByte "strcharat.three.b0.value" threeByte0Masked
      threeByte0Shifted <- emitAssign "strcharat.three.b0.shifted" i32Ty (LLVMShl threeByte0Value (LLVMIntLiteral 32 12))
      threeByte1 <- loadByte "strcharat.three.b1" cursor 1
      threeByte1Masked <- emitAssign "strcharat.three.b1.masked" i8Ty (LLVMAnd threeByte1 (LLVMIntLiteral 8 0x3F))
      threeByte1Value <- extendByte "strcharat.three.b1.value" threeByte1Masked
      threeByte1Shifted <- emitAssign "strcharat.three.b1.shifted" i32Ty (LLVMShl threeByte1Value (LLVMIntLiteral 32 6))
      threePrefix <- emitAssign "strcharat.three.prefix" i32Ty (LLVMOr threeByte0Shifted threeByte1Shifted)
      threeByte2 <- loadByte "strcharat.three.b2" cursor 2
      threeByte2Masked <- emitAssign "strcharat.three.b2.masked" i8Ty (LLVMAnd threeByte2 (LLVMIntLiteral 8 0x3F))
      threeByte2Value <- extendByte "strcharat.three.b2.value" threeByte2Masked
      threeScalar <- emitAssign "strcharat.three.scalar" i32Ty (LLVMOr threePrefix threeByte2Value)
      finishCurrentBlock (LLVMRet i32Ty threeScalar)
      startBlock fourByteScalar
      fourByte0Masked <- emitAssign "strcharat.four.b0.masked" i8Ty (LLVMAnd byte0 (LLVMIntLiteral 8 0x07))
      fourByte0Value <- extendByte "strcharat.four.b0.value" fourByte0Masked
      fourByte0Shifted <- emitAssign "strcharat.four.b0.shifted" i32Ty (LLVMShl fourByte0Value (LLVMIntLiteral 32 18))
      fourByte1 <- loadByte "strcharat.four.b1" cursor 1
      fourByte1Masked <- emitAssign "strcharat.four.b1.masked" i8Ty (LLVMAnd fourByte1 (LLVMIntLiteral 8 0x3F))
      fourByte1Value <- extendByte "strcharat.four.b1.value" fourByte1Masked
      fourByte1Shifted <- emitAssign "strcharat.four.b1.shifted" i32Ty (LLVMShl fourByte1Value (LLVMIntLiteral 32 12))
      fourPrefix <- emitAssign "strcharat.four.prefix" i32Ty (LLVMOr fourByte0Shifted fourByte1Shifted)
      fourByte2 <- loadByte "strcharat.four.b2" cursor 2
      fourByte2Masked <- emitAssign "strcharat.four.b2.masked" i8Ty (LLVMAnd fourByte2 (LLVMIntLiteral 8 0x3F))
      fourByte2Value <- extendByte "strcharat.four.b2.value" fourByte2Masked
      fourByte2Shifted <- emitAssign "strcharat.four.b2.shifted" i32Ty (LLVMShl fourByte2Value (LLVMIntLiteral 32 6))
      fourPrefix' <- emitAssign "strcharat.four.prefix2" i32Ty (LLVMOr fourPrefix fourByte2Shifted)
      fourByte3 <- loadByte "strcharat.four.b3" cursor 3
      fourByte3Masked <- emitAssign "strcharat.four.b3.masked" i8Ty (LLVMAnd fourByte3 (LLVMIntLiteral 8 0x3F))
      fourByte3Value <- extendByte "strcharat.four.b3.value" fourByte3Masked
      fourScalar <- emitAssign "strcharat.four.scalar" i32Ty (LLVMOr fourPrefix' fourByte3Value)
      finishCurrentBlock (LLVMRet i32Ty fourScalar)
  of
    Right function -> function
    Left err -> error ("internal native __string_char_at lowering failed: " ++ renderBackendLLVMError err)

nativeStringCharAtOptionFunction :: LLVMFunction
nativeStringCharAtOptionFunction =
  case
    lowerNativeFunction runtimeStringCharAtOptionName LLVMPtr [(LLVMPtr, "value"), (LLVMInt 64, "index")] $ \params -> do
      let value = requireNativeParam "value" params
          index = requireNativeParam "index" params
          i1Ty = LLVMInt 1
          i32Ty = LLVMInt 32
          i64Ty = LLVMInt 64
          allocateNone label = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 0)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 0) tagPtr
            pure cell
          allocateSome label char = do
            cell <-
              emitAssign
                (label ++ ".cell")
                LLVMPtr
                (LLVMCall runtimeMallocName [(i64Ty, LLVMIntLiteral 64 (toInteger (constructorObjectBytes 1)))])
            tagPtr <- emitGep (label ++ ".tag") cell constructorTagOffset
            emitStore i64Ty (LLVMIntLiteral 64 1) tagPtr
            valuePtr <- emitGep (label ++ ".value") cell (constructorFieldOffset 0)
            emitStore i32Ty char valuePtr
            pure cell
      boundsCheck <- freshBlock "strcharatopt.bounds"
      decode <- freshBlock "strcharatopt.decode"
      noneBlock <- freshBlock "strcharatopt.none"
      indexNegative <- emitAssign "strcharatopt.index.negative" i1Ty (LLVMICmpUgt index (LLVMIntLiteral 64 9223372036854775807))
      finishCurrentBlock (LLVMSwitch i1Ty indexNegative boundsCheck [(1, noneBlock)])
      startBlock boundsCheck
      lengthValue <-
        emitAssign
          "strcharatopt.length"
          i64Ty
          (LLVMCall runtimeStringLengthName [(LLVMPtr, value)])
      inRange <- emitAssign "strcharatopt.in.range" i1Ty (LLVMICmpUgt lengthValue index)
      finishCurrentBlock (LLVMSwitch i1Ty inRange noneBlock [(1, decode)])
      startBlock decode
      char <-
        emitAssign
          "strcharatopt.char"
          i32Ty
          ( LLVMCall
              runtimeStringCharAtName
              [(LLVMPtr, value), (i64Ty, index)]
          )
      some <- allocateSome "strcharatopt.some" char
      finishCurrentBlock (LLVMRet LLVMPtr some)
      startBlock noneBlock
      none <- allocateNone "strcharatopt.none"
      finishCurrentBlock (LLVMRet LLVMPtr none)
  of
    Right function -> function
    Left err -> error ("internal native __string_char_at_option lowering failed: " ++ renderBackendLLVMError err)

-- IO runtime primitives

ioPureName :: String
ioPureName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOPure

ioBindName :: String
ioBindName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOBind

ioMapName :: String
ioMapName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOMap

ioPutStrLnName :: String
ioPutStrLnName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOPutStrLn

ioGetLineName :: String
ioGetLineName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOGetLine

ioPutStrName :: String
ioPutStrName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOPutStr

ioReadFileName :: String
ioReadFileName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOReadFile

ioWriteFileName :: String
ioWriteFileName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOWriteFile

ioAppendFileName :: String
ioAppendFileName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOAppendFile

ioExitWithName :: String
ioExitWithName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOExitWith

ioNewIORefName :: String
ioNewIORefName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIONewIORef

ioReadIORefName :: String
ioReadIORefName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOReadIORef

ioWriteIORefName :: String
ioWriteIORefName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOWriteIORef

ioGetArgsName :: String
ioGetArgsName = PrimitiveInventory.nativeIOPrimitiveName PrimitiveInventory.PrimitiveIOGetArgs

nativeIOEntryName :: String -> String
nativeIOEntryName prim = prim ++ ".entry"

nativeIOWrapperName :: String -> String
nativeIOWrapperName prim = prim ++ ".wrapper"

nativeIOFunctions :: ProgramBase -> [LLVMFunction]
nativeIOFunctions _base =
  concatMap snd (checkedIOPrimitiveImplementations nativeIOPrimitiveImplementations)
  where
    nativeIOPrimitiveImplementations =
      [ (ioPureName, [ioPureEntry, ioPureWrapper]),
        (ioBindName, [ioBindEntry, ioBindWrapper]),
        (ioMapName, [ioMapEntry, ioMapWrapper]),
        (ioPutStrLnName, [ioPutStrLnEntry, ioPutStrLnWrapper]),
        (ioGetLineName, [ioGetLineEntry, ioGetLineWrapper]),
        (ioPutStrName, [ioPutStrEntry, ioPutStrWrapper]),
        (ioReadFileName, [ioReadFileEntry, ioReadFileWrapper]),
        (ioWriteFileName, [ioWriteFileEntry, ioWriteFileWrapper]),
        (ioAppendFileName, [ioAppendFileEntry, ioAppendFileWrapper]),
        (ioExitWithName, [ioExitWithEntry, ioExitWithWrapper]),
        (ioNewIORefName, [ioNewIORefEntry, ioNewIORefWrapper]),
        (ioReadIORefName, [ioReadIORefEntry, ioReadIORefWrapper]),
        (ioWriteIORefName, [ioWriteIORefEntry, ioWriteIORefWrapper]),
        (ioGetArgsName, [ioGetArgsEntry, ioGetArgsWrapper])
      ]

    checkedIOPrimitiveImplementations implementations
      | Set.null missing && Set.null extra = implementations
      | otherwise =
          error
            ( "internal native IO primitive coverage drift: missing "
                ++ show (Set.toAscList missing)
                ++ ", extra "
                ++ show (Set.toAscList extra)
            )
      where
        implementedNames = Set.fromList (map fst implementations)
        missing = PrimitiveInventory.nativeIOPrimitiveNames `Set.difference` implementedNames
        extra = implementedNames `Set.difference` PrimitiveInventory.nativeIOPrimitiveNames

    emitMallocLocal :: String -> Int -> LowerM LLVMOperand
    emitMallocLocal prefix size =
      emitAssign prefix LLVMPtr (LLVMCall runtimeMallocName [(LLVMInt 64, LLVMIntLiteral 64 (toInteger size))])

    emitPrintCStringLoop :: String -> LLVMOperand -> Bool -> LowerM ()
    emitPrintCStringLoop prefix str emitNewline = do
      let i8Ty = LLVMInt 8
      let i64Ty = LLVMInt 64
      curSlot <- emitAssign (prefix ++ ".slot") LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr str curSlot
      loopHeader <- freshBlock (prefix ++ ".header")
      loopBody <- freshBlock (prefix ++ ".body")
      loopNext <- freshBlock (prefix ++ ".next")
      loopDone <- freshBlock (prefix ++ ".done")
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      curPtr <- emitAssign (prefix ++ ".cur") LLVMPtr (LLVMLoad LLVMPtr curSlot)
      charPtr <- emitAssign (prefix ++ ".cptr") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 0)])
      charVal <- emitAssign (prefix ++ ".c") i8Ty (LLVMLoad i8Ty charPtr)
      isNull <- emitAssign (prefix ++ ".end") (LLVMInt 1) (LLVMICmpEq charVal (LLVMIntLiteral 8 0))
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isNull loopBody [(1, loopDone)])
      startBlock loopBody
      charI32 <- emitAssign (prefix ++ ".c32") (LLVMInt 32) (LLVMZext charVal (LLVMInt 32))
      _ <- emitPutchar charI32
      finishCurrentBlock (LLVMBr loopNext)
      startBlock loopNext
      nextPtr <- emitAssign (prefix ++ ".next") LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 1)])
      emitStore LLVMPtr nextPtr curSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopDone
      when emitNewline $ void (emitPutchar (LLVMIntLiteral 32 10))
      finishCurrentBlock (LLVMRet LLVMPtr LLVMNull)

    finalizeEntry :: String -> (LLVMOperand -> LowerM ()) -> LLVMFunction
    finalizeEntry prim body =
      case lowerNativeFunction (nativeIOEntryName prim) LLVMPtr [(LLVMPtr, "env")] $ \params -> do
        let envPtr = requireNativeParam "env" params
        body envPtr
        of
        Right fn -> fn { llvmFunctionPrivate = True }
        Left err -> error ("internal native " ++ prim ++ " entry lowering failed: " ++ renderBackendLLVMError err)

    finalizeWrapper :: String -> [(LLVMType, String)] -> LLVMFunction
    finalizeWrapper prim args =
      case lowerNativeFunction (nativeIOWrapperName prim) LLVMPtr args $ \params -> do
        let envSize = max 1 (length args * 8)
        closure <- emitMallocLocal (prim ++ ".closure") 16
        envPtr <- emitMallocLocal (prim ++ ".env") envSize
        forM_ (zip [0, 8 :: Int ..] args) $ \(offset, (ty, argName)) -> do
          let value = requireNativeParam argName params
          if offset == 0
            then emitStore ty value envPtr
            else do
              slot <- emitGep (prim ++ "." ++ argName) envPtr offset
              emitStore ty value slot
        codePtr <- emitGep (prim ++ ".code") closure 0
        emitStore LLVMPtr (LLVMGlobalRef LLVMPtr (nativeIOEntryName prim)) codePtr
        envSlot <- emitGep (prim ++ ".env.slot") closure 8
        emitStore LLVMPtr envPtr envSlot
        finishCurrentBlock (LLVMRet LLVMPtr closure)
        of
        Right fn -> fn { llvmFunctionPrivate = True }
        Left err -> error ("internal native " ++ prim ++ " wrapper lowering failed: " ++ renderBackendLLVMError err)

    runIOAction :: String -> LLVMOperand -> LowerM LLVMOperand
    runIOAction prefix action = do
      codePtrField <- emitGep (prefix ++ ".code") action 0
      code <- emitAssign (prefix ++ ".code") LLVMPtr (LLVMLoad LLVMPtr codePtrField)
      envField <- emitGep (prefix ++ ".env") action 8
      env <- emitAssign (prefix ++ ".env") LLVMPtr (LLVMLoad LLVMPtr envField)
      emitAssign (prefix ++ ".result") LLVMPtr (LLVMCallOperand code [(LLVMPtr, env)])

    callClosure1 :: String -> LLVMOperand -> LLVMOperand -> LowerM LLVMOperand
    callClosure1 prefix closure arg = do
      codePtrField <- emitGep (prefix ++ ".code") closure 0
      code <- emitAssign (prefix ++ ".code") LLVMPtr (LLVMLoad LLVMPtr codePtrField)
      envField <- emitGep (prefix ++ ".env") closure 8
      env <- emitAssign (prefix ++ ".env") LLVMPtr (LLVMLoad LLVMPtr envField)
      emitAssign (prefix ++ ".result") LLVMPtr (LLVMCallOperand code [(LLVMPtr, env), (LLVMPtr, arg)])

    ioPureEntry = finalizeEntry ioPureName $ \envPtr -> do
      valPtr <- emitGep "pure.val" envPtr 0
      val <- emitAssign "pure.val" LLVMPtr (LLVMLoad LLVMPtr valPtr)
      finishCurrentBlock (LLVMRet LLVMPtr val)
    ioPureWrapper = finalizeWrapper ioPureName [(LLVMPtr, "value")]

    ioBindEntry = finalizeEntry ioBindName $ \envPtr -> do
      actionPtr <- emitGep "bind.action" envPtr 0
      action <- emitAssign "bind.action" LLVMPtr (LLVMLoad LLVMPtr actionPtr)
      contPtr <- emitGep "bind.cont" envPtr 8
      cont <- emitAssign "bind.cont" LLVMPtr (LLVMLoad LLVMPtr contPtr)
      actionResult <- runIOAction "bind.action" action
      nextAction <- callClosure1 "bind.cont" cont actionResult
      result <- runIOAction "bind.next" nextAction
      finishCurrentBlock (LLVMRet LLVMPtr result)
    ioBindWrapper = finalizeWrapper ioBindName [(LLVMPtr, "action"), (LLVMPtr, "cont")]

    ioMapEntry = finalizeEntry ioMapName $ \envPtr -> do
      mapperPtr <- emitGep "map.mapper" envPtr 0
      mapper <- emitAssign "map.mapper" LLVMPtr (LLVMLoad LLVMPtr mapperPtr)
      actionPtr <- emitGep "map.action" envPtr 8
      action <- emitAssign "map.action" LLVMPtr (LLVMLoad LLVMPtr actionPtr)
      actionResult <- runIOAction "map.action" action
      mapped <- callClosure1 "map.mapper" mapper actionResult
      finishCurrentBlock (LLVMRet LLVMPtr mapped)
    ioMapWrapper = finalizeWrapper ioMapName [(LLVMPtr, "mapper"), (LLVMPtr, "action")]

    ioPutStrLnEntry = finalizeEntry ioPutStrLnName $ \envPtr -> do
      strPtr <- emitGep "putStrLn.str" envPtr 0
      str <- emitAssign "putStrLn.str" LLVMPtr (LLVMLoad LLVMPtr strPtr)
      emitPrintCStringLoop "putStrLn" str True
    ioPutStrLnWrapper = finalizeWrapper ioPutStrLnName [(LLVMPtr, "str")]

    ioGetLineEntry = finalizeEntry ioGetLineName $ \_envPtr -> do
      strPtr <- emitAssign "getLine.result" LLVMPtr (LLVMCall nativeReadLineName [])
      finishCurrentBlock (LLVMRet LLVMPtr strPtr)
    ioGetLineWrapper = finalizeWrapper ioGetLineName []

    ioPutStrEntry = finalizeEntry ioPutStrName $ \envPtr -> do
      strPtr <- emitGep "putStr.str" envPtr 0
      str <- emitAssign "putStr.str" LLVMPtr (LLVMLoad LLVMPtr strPtr)
      emitPrintCStringLoop "putStr" str False
    ioPutStrWrapper = finalizeWrapper ioPutStrName [(LLVMPtr, "str")]

    ioReadFileEntry = finalizeEntry ioReadFileName $ \envPtr -> do
      pathPtr <- emitGep "readFile.path" envPtr 0
      path <- emitAssign "readFile.path" LLVMPtr (LLVMLoad LLVMPtr pathPtr)
      strPtr <- emitAssign "readFile.result" LLVMPtr (LLVMCall nativeReadFileName [(LLVMPtr, path)])
      finishCurrentBlock (LLVMRet LLVMPtr strPtr)
    ioReadFileWrapper = finalizeWrapper ioReadFileName [(LLVMPtr, "path")]

    ioWriteFileEntry = finalizeEntry ioWriteFileName $ \envPtr -> do
      pathPtr <- emitGep "writeFile.path" envPtr 0
      path <- emitAssign "writeFile.path" LLVMPtr (LLVMLoad LLVMPtr pathPtr)
      contentsPtr <- emitGep "writeFile.contents" envPtr 8
      contents <- emitAssign "writeFile.contents" LLVMPtr (LLVMLoad LLVMPtr contentsPtr)
      _ <- emitAssign "writeFile.result" (LLVMInt 32) (LLVMCall nativeWriteFileName [(LLVMPtr, path), (LLVMPtr, contents)])
      finishCurrentBlock (LLVMRet LLVMPtr LLVMNull)
    ioWriteFileWrapper = finalizeWrapper ioWriteFileName [(LLVMPtr, "path"), (LLVMPtr, "contents")]

    ioAppendFileEntry = finalizeEntry ioAppendFileName $ \envPtr -> do
      pathPtr <- emitGep "appendFile.path" envPtr 0
      path <- emitAssign "appendFile.path" LLVMPtr (LLVMLoad LLVMPtr pathPtr)
      contentsPtr <- emitGep "appendFile.contents" envPtr 8
      contents <- emitAssign "appendFile.contents" LLVMPtr (LLVMLoad LLVMPtr contentsPtr)
      _ <- emitAssign "appendFile.result" (LLVMInt 32) (LLVMCall nativeAppendFileName [(LLVMPtr, path), (LLVMPtr, contents)])
      finishCurrentBlock (LLVMRet LLVMPtr LLVMNull)
    ioAppendFileWrapper = finalizeWrapper ioAppendFileName [(LLVMPtr, "path"), (LLVMPtr, "contents")]

    ioExitWithEntry = finalizeEntry ioExitWithName $ \envPtr -> do
      statusPtr <- emitGep "exitWith.status" envPtr 0
      status <- emitAssign "exitWith.status" (LLVMInt 64) (LLVMLoad (LLVMInt 64) statusPtr)
      _ <- emitAssign "exitWith.call" (LLVMInt 32) (LLVMCall nativeExitName [(LLVMInt 64, status)])
      finishCurrentBlock (LLVMRet LLVMPtr LLVMNull)
    ioExitWithWrapper = finalizeWrapper ioExitWithName [(LLVMInt 64, "status")]

    ioNewIORefEntry = finalizeEntry ioNewIORefName $ \envPtr -> do
      valPtr <- emitGep "newIORef.val" envPtr 0
      val <- emitAssign "newIORef.val" LLVMPtr (LLVMLoad LLVMPtr valPtr)
      cell <- emitMallocLocal "newIORef.cell" 8
      emitStore LLVMPtr val cell
      finishCurrentBlock (LLVMRet LLVMPtr cell)
    ioNewIORefWrapper = finalizeWrapper ioNewIORefName [(LLVMPtr, "value")]

    ioReadIORefEntry = finalizeEntry ioReadIORefName $ \envPtr -> do
      refPtr <- emitGep "readIORef.ref" envPtr 0
      ref <- emitAssign "readIORef.ref" LLVMPtr (LLVMLoad LLVMPtr refPtr)
      val <- emitAssign "readIORef.val" LLVMPtr (LLVMLoad LLVMPtr ref)
      finishCurrentBlock (LLVMRet LLVMPtr val)
    ioReadIORefWrapper = finalizeWrapper ioReadIORefName [(LLVMPtr, "ref")]

    ioWriteIORefEntry = finalizeEntry ioWriteIORefName $ \envPtr -> do
      refPtr <- emitGep "writeIORef.ref" envPtr 0
      ref <- emitAssign "writeIORef.ref" LLVMPtr (LLVMLoad LLVMPtr refPtr)
      valPtr <- emitGep "writeIORef.val" envPtr 8
      val <- emitAssign "writeIORef.val" LLVMPtr (LLVMLoad LLVMPtr valPtr)
      emitStore LLVMPtr val ref
      finishCurrentBlock (LLVMRet LLVMPtr LLVMNull)
    ioWriteIORefWrapper = finalizeWrapper ioWriteIORefName [(LLVMPtr, "ref"), (LLVMPtr, "value")]

    ioGetArgsEntry = finalizeEntry ioGetArgsName $ \_envPtr -> do
      argsPtr <- emitAssign "getArgs.result" LLVMPtr (LLVMCall nativeGetArgsName [])
      let i8Ty = LLVMInt 8
      let i64Ty = LLVMInt 64
      curSlot <- emitAssign "getArgs.cur" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      emitStore LLVMPtr argsPtr curSlot
      accSlot <- emitAssign "getArgs.acc" LLVMPtr (LLVMAlloca LLVMPtr (LLVMIntLiteral 64 1))
      nilCell <- emitMallocLocal "getArgs.nil" 8
      tagPtr0 <- emitGep "getArgs.nil.tag" nilCell 0
      emitStore i64Ty (LLVMIntLiteral 64 0) tagPtr0
      emitStore LLVMPtr nilCell accSlot
      loopHeader <- freshBlock "getArgs.header"
      loopBody <- freshBlock "getArgs.body"
      loopNext <- freshBlock "getArgs.next"
      loopDone <- freshBlock "getArgs.done"
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopHeader
      curPtr <- emitAssign "getArgs.cur" LLVMPtr (LLVMLoad LLVMPtr curSlot)
      strPtr <- emitAssign "getArgs.str" LLVMPtr (LLVMLoad LLVMPtr curPtr)
      isNull <- emitAssign "getArgs.isnull" (LLVMInt 1) (LLVMICmpEq strPtr LLVMNull)
      finishCurrentBlock (LLVMSwitch (LLVMInt 1) isNull loopBody [(1, loopDone)])
      startBlock loopBody
      consCell <- emitMallocLocal "getArgs.cons" 24
      tagPtr1 <- emitGep "getArgs.cons.tag" consCell 0
      emitStore i64Ty (LLVMIntLiteral 64 1) tagPtr1
      headPtr <- emitGep "getArgs.cons.head" consCell 8
      emitStore LLVMPtr strPtr headPtr
      acc <- emitAssign "getArgs.acc" LLVMPtr (LLVMLoad LLVMPtr accSlot)
      tailPtr <- emitGep "getArgs.cons.tail" consCell 16
      emitStore LLVMPtr acc tailPtr
      emitStore LLVMPtr consCell accSlot
      finishCurrentBlock (LLVMBr loopNext)
      startBlock loopNext
      nextPtr <- emitAssign "getArgs.next" LLVMPtr (LLVMGetElementPtr i8Ty curPtr [(i64Ty, LLVMIntLiteral 64 8)])
      emitStore LLVMPtr nextPtr curSlot
      finishCurrentBlock (LLVMBr loopHeader)
      startBlock loopDone
      result <- emitAssign "getArgs.result.list" LLVMPtr (LLVMLoad LLVMPtr accSlot)
      _ <- emitAssign "getArgs.free" (LLVMInt 32) (LLVMCall nativeFreeArgsName [(LLVMPtr, argsPtr)])
      finishCurrentBlock (LLVMRet LLVMPtr result)
    ioGetArgsWrapper = finalizeWrapper ioGetArgsName []

-- | Names of IO runtime primitives that are handled specially.
ioPrimitiveNames :: Set.Set String
ioPrimitiveNames = PrimitiveInventory.nativeIOPrimitiveNames

nativePrimitiveNames :: Set.Set String
nativePrimitiveNames =
  PrimitiveInventory.nativeLowerablePrimitiveNames `Set.difference` ioPrimitiveNames

resolveIOPrimitiveAsValue :: BackendType -> String -> LowerM LowerValue
resolveIOPrimitiveAsValue ty name = do
  let wrapperName = nativeIOWrapperName name
  if isFunctionLikeBackendType ty
    then pure (LowerValue ty LLVMPtr (LLVMGlobalRef LLVMPtr wrapperName) LowerRuntimeValue Nothing)
    else do
      result <- emitAssign "io.prim" LLVMPtr (LLVMCall wrapperName [])
      pure (LowerValue ty LLVMPtr result LowerRuntimeValue Nothing)

resolveNativePrimitiveAsValue :: BackendType -> String -> LowerM LowerValue
resolveNativePrimitiveAsValue ty name
  | isFunctionLikeBackendType ty =
      pure (functionPointerValue ty (LLVMGlobalRef LLVMPtr name))
  | otherwise =
      liftEither (BackendLLVMUnknownFunction name)

-- | LLVM-level names of IO wrapper functions generated by 'nativeIOFunctions'.
ioWrapperNames :: Set.Set String
ioWrapperNames = Set.map nativeIOWrapperName ioPrimitiveNames

-- | Check whether an LLVM function references any IO wrapper function.
functionReferencesIOWrapper :: LLVMFunction -> Bool
functionReferencesIOWrapper =
  functionReferencesGlobalNames ioWrapperNames

functionReferencesGlobalNames :: Set.Set String -> LLVMFunction -> Bool
functionReferencesGlobalNames names fn =
  any blockReferencesName (llvmFunctionBlocks fn)
  where
    blockReferencesName block =
      any instrReferencesName (llvmBlockInstructions block)
        || terminatorReferencesName (llvmBlockTerminator block)
    instrReferencesName (LLVMAssign _ _ expr) = exprReferencesName expr
    instrReferencesName (LLVMStore _ src dst) = opReferencesName src || opReferencesName dst
    instrReferencesName (LLVMComment _) = False
    exprReferencesName (LLVMCall name args) =
      Set.member name names || any (opReferencesName . snd) args
    exprReferencesName (LLVMCallVarArgs name _ args) =
      Set.member name names || any (opReferencesName . snd) args
    exprReferencesName (LLVMCallOperand op args) =
      opReferencesName op || any (opReferencesName . snd) args
    exprReferencesName (LLVMGetElementPtr _ base idxs) =
      opReferencesName base || any (opReferencesName . snd) idxs
    exprReferencesName (LLVMLoad _ op) = opReferencesName op
    exprReferencesName (LLVMAlloca _ op) = opReferencesName op
    exprReferencesName (LLVMAnd a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMOr a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMShl a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMAdd a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMSub a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMICmpEq a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMICmpUgt a b) = opReferencesName a || opReferencesName b
    exprReferencesName (LLVMZext op _) = opReferencesName op
    exprReferencesName (LLVMPhi _ arms) = any (opReferencesName . fst) arms
    opReferencesName (LLVMGlobalRef _ name) = Set.member name names
    opReferencesName _ = False
    terminatorReferencesName (LLVMRet _ op) = opReferencesName op
    terminatorReferencesName (LLVMBr _) = False
    terminatorReferencesName (LLVMSwitch _ op _ _) = opReferencesName op
    terminatorReferencesName LLVMUnreachable = False

lowerNativeFunction ::
  String ->
  LLVMType ->
  [(LLVMType, String)] ->
  (Map String LLVMOperand -> LowerM ()) ->
  Either BackendLLVMError LLVMFunction
lowerNativeFunction name returnTy params buildBody = do
  blocks <- evalStateT (buildBody paramOperands >> gets (reverse . fsCompletedBlocks)) (initialFunctionState initialIdentityGenerator)
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
    (LLVMCallVarArgs nativePrintfName [LLVMPtr] ((LLVMPtr, LLVMGlobalRef LLVMPtr formatGlobal) : args))

emitPrintStringGlobal :: String -> LowerM LLVMOperand
emitPrintStringGlobal globalName =
  emitPrintf nativeFmtStringName [(LLVMPtr, LLVMGlobalRef LLVMPtr globalName)]

emitPutchar :: LLVMOperand -> LowerM LLVMOperand
emitPutchar charOperand =
  emitAssign "putchar" (LLVMInt 32) (LLVMCall nativePutcharName [(LLVMInt 32, charOperand)])

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

shouldLowerReachableBinding :: ReferencedFunctions -> BindingInfo -> Bool
shouldLowerReachableBinding referencedFunctions binding =
  null (ffTypeBinders form)
    && ( biExportedAsMain binding
           || canEmitDirectReachableFunction binding
           || (Set.member (bindingInfoRef binding) (rfBindings referencedFunctions) && canEmitReferencedFunctionForm form)
       )
  where
    form = biForm binding

canEmitDirectReachableFunction :: BindingInfo -> Bool
canEmitDirectReachableFunction binding
  | canEmitRawFunctionPointerReturningForm form =
      True
  | not (requiresInlineCall form) =
      canEmitFunctionForm form
  | otherwise =
      canEmitReferencedFunctionForm form && functionFormCallsGlobal binding
  where
    form = biForm binding

canEmitRawFunctionPointerReturningForm :: FunctionForm -> Bool
canEmitRawFunctionPointerReturningForm form =
  isFirstOrderFunctionPointerType (ffReturnType form)
    && canEmitReferencedFunctionForm form
    && functionFormReturnsRawFunctionPointerAlias form
    && not (containsInlineOnlyEvidenceParameterCall form)
    && all inlineOnlyParamCanTravel (indexed (ffParams form))
  where
    inlineOnlyParamCanTravel (index0, (_, paramTy)) =
      not (isInlineOnlyFunctionParameter (ffEvidenceParams form) index0 paramTy)
        || isFirstOrderFunctionPointerType paramTy

functionFormReturnsRawFunctionPointerAlias :: FunctionForm -> Bool
functionFormReturnsRawFunctionPointerAlias form =
  rawFunctionPointerAliasValueKind (functionFormParamValueKinds form) (ffBody form) == Just LowerFunctionPointer

rawFunctionPointerAliasValueKind :: LocalValueKinds -> BackendExpr -> Maybe LowerValueKind
rawFunctionPointerAliasValueKind kinds =
  \case
    BackendVarWithIdentity ty mbIdentity _name
      | isFunctionLikeBackendType ty ->
          lookupLocalValueKind mbIdentity kinds
    expr0@(BackendTyApp ty fun _)
      | isFunctionLikeBackendType ty ->
          case collectTyApps expr0 of
            (BackendVarWithIdentity _ mbIdentity _name, _) ->
              lookupLocalValueKind mbIdentity kinds
            _ ->
              rawFunctionPointerAliasValueKind kinds fun
    BackendLetWithIdentity ty mbIdentity _name _ rhs body
      | isFunctionLikeBackendType ty ->
          let kindsForBody =
                case rawFunctionPointerAliasValueKind kinds rhs of
                  Just kind -> bindLocalValueKind mbIdentity kind kinds
                  Nothing -> deleteLocalValueKind mbIdentity kinds
           in rawFunctionPointerAliasValueKind kindsForBody body
    _ ->
      Nothing

functionFormCallsGlobal :: BindingInfo -> Bool
functionFormCallsGlobal binding =
  go (termBoundKeyRefs [mbIdentity | (mbIdentity, _, _) <- functionFormParamTriples form]) (ffBody form)
  where
    form = biForm binding

    go bound expr =
      case collectCall expr of
        Just (BackendVarWithIdentity _ mbIdentity calleeName, _, _)
          | globalReferenceMatchesBinding binding mbIdentity calleeName,
            Set.null (termReferenceKeys mbIdentity `Set.intersection` bound) ->
              True
        _ -> childCalls bound expr

    childCalls bound =
      \case
        BackendVarWithIdentity {} -> False
        BackendLit {} -> False
        BackendLamWithIdentity _ mbIdentity _paramName _ body ->
          go (Set.union (termBoundKeys mbIdentity) bound) body
        BackendApp _ fun arg ->
          go bound fun || go bound arg
        BackendLetWithIdentity _ mbIdentity _localName _ rhs body ->
          go bound rhs || go (Set.union (termBoundKeys mbIdentity) bound) body
        BackendTyAbs _ _ _ body ->
          go bound body
        BackendTyApp _ fun _ ->
          go bound fun
        BackendConstructWithIdentity _ _ _ args ->
          any (go bound) args
        BackendCase _ scrutinee alternatives ->
          go bound scrutinee || any (alternativeCalls bound) (NE.toList alternatives)
        BackendRoll _ payload ->
          go bound payload
        BackendUnroll _ payload ->
          go bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          any (go bound . backendClosureCaptureExpr) captures
            || go (Set.union (termBoundKeyRefs (map fst closureRefs)) bound) body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          go bound fun || any (go bound) args

    alternativeCalls bound (BackendAlternative pattern0 body) =
      go (Set.union bound (patternTermBoundKeys pattern0)) body

globalReferenceMatchesBinding :: BindingInfo -> Maybe IdDetails -> String -> Bool
globalReferenceMatchesBinding binding mbIdentity calleeName =
  symbolRefMatches (biIdentity binding) (biName binding) (backendVarSymbolIdentity mbIdentity) calleeName

patternBinderRefs :: BackendPattern -> [(Maybe IdDetails, String)]
patternBinderRefs = \case
  BackendDefaultPattern -> []
  BackendConstructorPatternWithBinderIdentities _ _ binders ->
    [(backendPatternBinderIdentity binder, backendPatternBinderName binder) | binder <- binders]

patternBinderUsedBy :: Set TermBoundKey -> BackendPatternBinder -> Bool
patternBinderUsedBy usedKeys binder =
  not (Set.null (termBoundKeys (backendPatternBinderIdentity binder) `Set.intersection` usedKeys))

shouldLowerSpecialization :: ReferencedFunctions -> Specialization -> Bool
shouldLowerSpecialization referencedFunctions specialization =
  (not (requiresInlineCall form) && canEmitFunctionForm form)
    || (Set.member (spBindingRef specialization) (rfGeneratedBindings referencedFunctions) && canEmitReferencedFunctionForm form)
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
  all (\(index0, (_, paramTy)) -> canEmitFunctionParameter (ffEvidenceParams form) index0 paramTy) (indexed (ffParams form))

canEmitFunctionParameter :: Set Int -> Int -> BackendType -> Bool
canEmitFunctionParameter evidenceParams index0 paramTy
  | isFunctionLikeBackendType paramTy =
      isEvidenceParameter evidenceParams index0 paramTy || isFirstOrderFunctionPointerType paramTy
  | otherwise = True

runtimeAndName :: String
runtimeAndName =
  PrimitiveInventory.nativeAndPrimitiveName

runtimeStringLengthName :: String
runtimeStringLengthName =
  PrimitiveInventory.stringLengthPrimitiveName

runtimeStringIsEmptyName :: String
runtimeStringIsEmptyName =
  PrimitiveInventory.stringIsEmptyPrimitiveName

runtimeStringContainsCharName :: String
runtimeStringContainsCharName =
  PrimitiveInventory.stringContainsCharPrimitiveName

runtimeStringContainsName :: String
runtimeStringContainsName =
  PrimitiveInventory.stringContainsPrimitiveName

runtimeStringEqualsName :: String
runtimeStringEqualsName =
  PrimitiveInventory.stringEqualsPrimitiveName

runtimeStringStartsWithName :: String
runtimeStringStartsWithName =
  PrimitiveInventory.stringStartsWithPrimitiveName

runtimeStringEndsWithName :: String
runtimeStringEndsWithName =
  PrimitiveInventory.stringEndsWithPrimitiveName

runtimeStringAppendName :: String
runtimeStringAppendName =
  PrimitiveInventory.stringAppendPrimitiveName

runtimeStringReplaceCharName :: String
runtimeStringReplaceCharName =
  PrimitiveInventory.stringReplaceCharPrimitiveName

runtimeStringReplaceName :: String
runtimeStringReplaceName =
  PrimitiveInventory.stringReplacePrimitiveName

runtimeStringIndexOfCharName :: String
runtimeStringIndexOfCharName =
  PrimitiveInventory.stringIndexOfCharPrimitiveName

runtimeStringIndexOfName :: String
runtimeStringIndexOfName =
  PrimitiveInventory.stringIndexOfPrimitiveName

runtimeStringSplitName :: String
runtimeStringSplitName =
  PrimitiveInventory.stringSplitPrimitiveName

runtimeStringJoinName :: String
runtimeStringJoinName =
  PrimitiveInventory.stringJoinPrimitiveName

runtimeStringSplitCharName :: String
runtimeStringSplitCharName =
  PrimitiveInventory.stringSplitCharPrimitiveName

runtimeStringCompareName :: String
runtimeStringCompareName =
  PrimitiveInventory.stringComparePrimitiveName

runtimeStringFromCharName :: String
runtimeStringFromCharName =
  PrimitiveInventory.stringFromCharPrimitiveName

runtimeStringFromIntName :: String
runtimeStringFromIntName =
  PrimitiveInventory.stringFromIntPrimitiveName

runtimeStringFromBoolName :: String
runtimeStringFromBoolName =
  PrimitiveInventory.stringFromBoolPrimitiveName

runtimeStringFromNatName :: String
runtimeStringFromNatName =
  PrimitiveInventory.stringFromNatPrimitiveName

runtimeStringFromListName :: String
runtimeStringFromListName =
  PrimitiveInventory.stringFromListPrimitiveName

runtimeStringToListName :: String
runtimeStringToListName =
  PrimitiveInventory.stringToListPrimitiveName

runtimeStringDropName :: String
runtimeStringDropName =
  PrimitiveInventory.stringDropPrimitiveName

runtimeStringTakeName :: String
runtimeStringTakeName =
  PrimitiveInventory.stringTakePrimitiveName

runtimeStringSliceName :: String
runtimeStringSliceName =
  PrimitiveInventory.stringSlicePrimitiveName

runtimeStringCharAtName :: String
runtimeStringCharAtName =
  PrimitiveInventory.stringCharAtPrimitiveName

runtimeStringCharAtOptionName :: String
runtimeStringCharAtOptionName =
  PrimitiveInventory.stringCharAtOptionPrimitiveName

runtimeCharIsDigitName :: String
runtimeCharIsDigitName =
  PrimitiveInventory.charIsDigitPrimitiveName

runtimeCharIsAsciiLowerName :: String
runtimeCharIsAsciiLowerName =
  PrimitiveInventory.charIsAsciiLowerPrimitiveName

runtimeCharIsAsciiUpperName :: String
runtimeCharIsAsciiUpperName =
  PrimitiveInventory.charIsAsciiUpperPrimitiveName

runtimeCharIsAsciiAlphaName :: String
runtimeCharIsAsciiAlphaName =
  PrimitiveInventory.charIsAsciiAlphaPrimitiveName

runtimeCharIsAsciiAlphaNumName :: String
runtimeCharIsAsciiAlphaNumName =
  PrimitiveInventory.charIsAsciiAlphaNumPrimitiveName

runtimeCharIsAsciiIdentifierStartName :: String
runtimeCharIsAsciiIdentifierStartName =
  PrimitiveInventory.charIsAsciiIdentifierStartPrimitiveName

runtimeCharIsAsciiIdentifierContinueName :: String
runtimeCharIsAsciiIdentifierContinueName =
  PrimitiveInventory.charIsAsciiIdentifierContinuePrimitiveName

runtimeCharIsAsciiWhitespaceName :: String
runtimeCharIsAsciiWhitespaceName =
  PrimitiveInventory.charIsAsciiWhitespacePrimitiveName

runtimeCharIsAsciiPunctuationName :: String
runtimeCharIsAsciiPunctuationName =
  PrimitiveInventory.charIsAsciiPunctuationPrimitiveName

runtimeCharIsAsciiPrintableName :: String
runtimeCharIsAsciiPrintableName =
  PrimitiveInventory.charIsAsciiPrintablePrimitiveName

runtimeCharIsAsciiHexDigitName :: String
runtimeCharIsAsciiHexDigitName =
  PrimitiveInventory.charIsAsciiHexDigitPrimitiveName

runtimeCharIsAsciiLineBreakName :: String
runtimeCharIsAsciiLineBreakName =
  PrimitiveInventory.charIsAsciiLineBreakPrimitiveName

runtimeCharIsAsciiControlName :: String
runtimeCharIsAsciiControlName =
  PrimitiveInventory.charIsAsciiControlPrimitiveName

runtimeCharToAsciiLowerName :: String
runtimeCharToAsciiLowerName =
  PrimitiveInventory.charToAsciiLowerPrimitiveName

runtimeCharToAsciiUpperName :: String
runtimeCharToAsciiUpperName =
  PrimitiveInventory.charToAsciiUpperPrimitiveName

runtimeStringToAsciiLowerName :: String
runtimeStringToAsciiLowerName =
  PrimitiveInventory.stringToAsciiLowerPrimitiveName

runtimeStringToAsciiUpperName :: String
runtimeStringToAsciiUpperName =
  PrimitiveInventory.stringToAsciiUpperPrimitiveName

runtimeMallocName :: String
runtimeMallocName =
  "malloc"

runtimeDeclarations :: ProgramBase -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [LLVMDeclaration]
runtimeDeclarations base needsStringLength needsStringIsEmpty needsStringContainsChar needsStringContains needsStringEquals needsStringStartsWith needsStringEndsWith needsStringAppend needsStringReplaceChar needsStringReplace needsStringIndexOfChar needsStringIndexOf needsStringSplit needsStringJoin needsStringSplitChar needsStringCompare needsStringFromChar needsStringFromInt needsStringFromBool needsStringFromNat needsStringFromList needsStringToList needsStringDrop needsStringTake needsStringSlice needsStringCharAt needsStringCharAtOption needsCharIsDigit needsCharIsAsciiLower needsCharIsAsciiUpper needsCharIsAsciiAlpha needsCharIsAsciiAlphaNum needsCharIsAsciiIdentifierStart needsCharIsAsciiIdentifierContinue needsCharIsAsciiWhitespace needsCharIsAsciiPunctuation needsCharIsAsciiPrintable needsCharIsAsciiHexDigit needsCharIsAsciiLineBreak needsCharIsAsciiControl needsCharToAsciiLower needsCharToAsciiUpper needsStringToAsciiLower needsStringToAsciiUpper =
  [ LLVMDeclaration runtimeMallocName LLVMPtr [LLVMInt 64] False
    | runtimeNameAvailable runtimeMallocName
  ]
    ++ [ LLVMDeclaration runtimeAndName (LLVMInt 1) [LLVMInt 1, LLVMInt 1] False
         | runtimeNameAvailable runtimeAndName
       ]
    ++ [ LLVMDeclaration runtimeStringLengthName (LLVMInt 64) [LLVMPtr] False
         | needsStringLength,
           runtimeNameAvailable runtimeStringLengthName
       ]
    ++ [ LLVMDeclaration runtimeStringIsEmptyName (LLVMInt 1) [LLVMPtr] False
         | needsStringIsEmpty,
           runtimeNameAvailable runtimeStringIsEmptyName
       ]
    ++ [ LLVMDeclaration runtimeStringContainsCharName (LLVMInt 1) [LLVMPtr, LLVMInt 32] False
         | needsStringContainsChar,
           runtimeNameAvailable runtimeStringContainsCharName
       ]
    ++ [ LLVMDeclaration runtimeStringContainsName (LLVMInt 1) [LLVMPtr, LLVMPtr] False
         | needsStringContains,
           runtimeNameAvailable runtimeStringContainsName
       ]
    ++ [ LLVMDeclaration runtimeStringEqualsName (LLVMInt 1) [LLVMPtr, LLVMPtr] False
         | needsStringEquals,
           runtimeNameAvailable runtimeStringEqualsName
       ]
    ++ [ LLVMDeclaration runtimeStringStartsWithName (LLVMInt 1) [LLVMPtr, LLVMPtr] False
         | needsStringStartsWith,
           runtimeNameAvailable runtimeStringStartsWithName
       ]
    ++ [ LLVMDeclaration runtimeStringEndsWithName (LLVMInt 1) [LLVMPtr, LLVMPtr] False
         | needsStringEndsWith,
           runtimeNameAvailable runtimeStringEndsWithName
       ]
    ++ [ LLVMDeclaration runtimeStringAppendName LLVMPtr [LLVMPtr, LLVMPtr] False
         | needsStringAppend,
           runtimeNameAvailable runtimeStringAppendName
       ]
    ++ [ LLVMDeclaration runtimeStringReplaceCharName LLVMPtr [LLVMPtr, LLVMInt 32, LLVMInt 32] False
         | needsStringReplaceChar,
           runtimeNameAvailable runtimeStringReplaceCharName
       ]
    ++ [ LLVMDeclaration runtimeStringReplaceName LLVMPtr [LLVMPtr, LLVMPtr, LLVMPtr] False
         | needsStringReplace,
           runtimeNameAvailable runtimeStringReplaceName
       ]
    ++ [ LLVMDeclaration runtimeStringIndexOfCharName LLVMPtr [LLVMPtr, LLVMInt 32] False
         | needsStringIndexOfChar,
           runtimeNameAvailable runtimeStringIndexOfCharName
       ]
    ++ [ LLVMDeclaration runtimeStringIndexOfName LLVMPtr [LLVMPtr, LLVMPtr] False
         | needsStringIndexOf,
           runtimeNameAvailable runtimeStringIndexOfName
       ]
    ++ [ LLVMDeclaration runtimeStringSplitName LLVMPtr [LLVMPtr, LLVMPtr] False
         | needsStringSplit,
           runtimeNameAvailable runtimeStringSplitName
       ]
    ++ [ LLVMDeclaration runtimeStringJoinName LLVMPtr [LLVMPtr, LLVMPtr] False
         | needsStringJoin,
           runtimeNameAvailable runtimeStringJoinName
       ]
    ++ [ LLVMDeclaration runtimeStringSplitCharName LLVMPtr [LLVMPtr, LLVMInt 32] False
         | needsStringSplitChar,
           runtimeNameAvailable runtimeStringSplitCharName
       ]
    ++ [ LLVMDeclaration runtimeStringCompareName (LLVMInt 64) [LLVMPtr, LLVMPtr] False
         | needsStringCompare,
           runtimeNameAvailable runtimeStringCompareName
       ]
    ++ [ LLVMDeclaration runtimeStringFromCharName LLVMPtr [LLVMInt 32] False
         | needsStringFromChar,
           runtimeNameAvailable runtimeStringFromCharName
       ]
    ++ [ LLVMDeclaration runtimeStringFromIntName LLVMPtr [LLVMInt 64] False
         | needsStringFromInt,
           runtimeNameAvailable runtimeStringFromIntName
       ]
    ++ [ LLVMDeclaration nativeSprintfName (LLVMInt 32) [LLVMPtr, LLVMPtr] True
         | needsStringFromInt || needsStringFromNat
       ]
    ++ [ LLVMDeclaration runtimeStringFromBoolName LLVMPtr [LLVMInt 1] False
         | needsStringFromBool,
           runtimeNameAvailable runtimeStringFromBoolName
       ]
    ++ [ LLVMDeclaration runtimeStringFromNatName LLVMPtr [LLVMPtr] False
         | needsStringFromNat,
           runtimeNameAvailable runtimeStringFromNatName
       ]
    ++ [ LLVMDeclaration runtimeStringFromListName LLVMPtr [LLVMPtr] False
         | needsStringFromList,
           runtimeNameAvailable runtimeStringFromListName
       ]
    ++ [ LLVMDeclaration runtimeStringToListName LLVMPtr [LLVMPtr] False
         | needsStringToList,
           runtimeNameAvailable runtimeStringToListName
       ]
    ++ [ LLVMDeclaration runtimeStringDropName LLVMPtr [LLVMPtr, LLVMInt 64] False
         | needsStringDrop,
           runtimeNameAvailable runtimeStringDropName
       ]
    ++ [ LLVMDeclaration runtimeStringTakeName LLVMPtr [LLVMPtr, LLVMInt 64] False
         | needsStringTake,
           runtimeNameAvailable runtimeStringTakeName
       ]
    ++ [ LLVMDeclaration runtimeStringSliceName LLVMPtr [LLVMPtr, LLVMInt 64, LLVMInt 64] False
         | needsStringSlice,
           runtimeNameAvailable runtimeStringSliceName
       ]
    ++ [ LLVMDeclaration runtimeStringCharAtName (LLVMInt 32) [LLVMPtr, LLVMInt 64] False
         | needsStringCharAt,
           runtimeNameAvailable runtimeStringCharAtName
       ]
    ++ [ LLVMDeclaration runtimeStringCharAtOptionName LLVMPtr [LLVMPtr, LLVMInt 64] False
         | needsStringCharAtOption,
           runtimeNameAvailable runtimeStringCharAtOptionName
       ]
    ++ [ LLVMDeclaration runtimeCharIsDigitName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsDigit,
           runtimeNameAvailable runtimeCharIsDigitName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiLowerName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiLower,
           runtimeNameAvailable runtimeCharIsAsciiLowerName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiUpperName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiUpper,
           runtimeNameAvailable runtimeCharIsAsciiUpperName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiAlphaName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiAlpha,
           runtimeNameAvailable runtimeCharIsAsciiAlphaName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiAlphaNumName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiAlphaNum,
           runtimeNameAvailable runtimeCharIsAsciiAlphaNumName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiIdentifierStartName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiIdentifierStart,
           runtimeNameAvailable runtimeCharIsAsciiIdentifierStartName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiIdentifierContinueName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiIdentifierContinue,
           runtimeNameAvailable runtimeCharIsAsciiIdentifierContinueName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiWhitespaceName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiWhitespace,
           runtimeNameAvailable runtimeCharIsAsciiWhitespaceName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiPunctuationName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiPunctuation,
           runtimeNameAvailable runtimeCharIsAsciiPunctuationName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiPrintableName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiPrintable,
           runtimeNameAvailable runtimeCharIsAsciiPrintableName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiHexDigitName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiHexDigit,
           runtimeNameAvailable runtimeCharIsAsciiHexDigitName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiLineBreakName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiLineBreak,
           runtimeNameAvailable runtimeCharIsAsciiLineBreakName
       ]
    ++ [ LLVMDeclaration runtimeCharIsAsciiControlName (LLVMInt 1) [LLVMInt 32] False
         | needsCharIsAsciiControl,
           runtimeNameAvailable runtimeCharIsAsciiControlName
       ]
    ++ [ LLVMDeclaration runtimeCharToAsciiLowerName (LLVMInt 32) [LLVMInt 32] False
         | needsCharToAsciiLower,
           runtimeNameAvailable runtimeCharToAsciiLowerName
       ]
    ++ [ LLVMDeclaration runtimeCharToAsciiUpperName (LLVMInt 32) [LLVMInt 32] False
         | needsCharToAsciiUpper,
           runtimeNameAvailable runtimeCharToAsciiUpperName
       ]
    ++ [ LLVMDeclaration runtimeStringToAsciiLowerName LLVMPtr [LLVMPtr] False
         | needsStringToAsciiLower,
           runtimeNameAvailable runtimeStringToAsciiLowerName
       ]
    ++ [ LLVMDeclaration runtimeStringToAsciiUpperName LLVMPtr [LLVMPtr] False
         | needsStringToAsciiUpper,
           runtimeNameAvailable runtimeStringToAsciiUpperName
       ]
  where
    bindingNames = programBindingRuntimeNames base
    runtimeNameAvailable name = Set.notMember name bindingNames

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
      (generatorAfterBindings, bindingInfos) =
        mapAccumL
          bindingInfo
          (identityGeneratorAfter (generatedIdentitiesInBackendProgram program))
          bindings
      (generatorAfterData, dataRuntimes) =
        mapAccumL dataRuntime generatorAfterBindings dataDecls
      constructors =
        concatMap drConstructors dataRuntimes
  pure
        ProgramBase
          { pbBindingsByIdentity =
              Map.fromList
                [ (identity, info)
                | info <- bindingInfos,
                  Just identity <- [biIdentity info]
                ],
            pbBindingsByRef =
              Map.fromList [(bindingInfoRef info, info) | info <- bindingInfos],
            pbBindingOrder = map bindingInfoRef bindingInfos,
            pbConstructorsByIdentity =
              Map.fromList
                [ (identity, ctor)
                | ctor <- constructors,
                  Just identity <- [backendConstructorIdentity (crConstructor ctor)]
                ],
            pbDataByIdentity =
              Map.fromList
                [ (identity, dataRuntime0)
                | dataRuntime0 <- dataRuntimes,
                  Just identity <- [backendDataIdentity (drData dataRuntime0)]
                ],
            pbIdentityGenerator = generatorAfterData
          }

bindingInfo :: IdentityGenerator -> BackendBinding -> (IdentityGenerator, BindingInfo)
bindingInfo generator binding =
  ( generator'',
    BindingInfo
      { biRef = bindingRef,
        biIdentity = backendBindingIdentity binding,
        biName = backendBindingName binding,
        biForm =
          markFunctionFormEvidenceParams
            (backendBindingEvidenceParamIndices binding)
            form,
        biExportedAsMain = backendBindingExportedAsMain binding
      }
  )
  where
    (form, generator') =
      functionFormFromExpectedWithGenerator generator (backendBindingType binding) (backendBindingExpr binding)
    (bindingRef, generator'') =
      case backendBindingIdentity binding of
        Just identity -> (backendBindingRefFromIdentity identity, generator')
        Nothing ->
          let (identity, generatorNext) = freshIdentity generator'
           in (backendBindingRefFromGenerated identity (backendBindingName binding), generatorNext)

programBindingRuntimeNames :: ProgramBase -> Set String
programBindingRuntimeNames base =
  Set.fromList (map biName (Map.elems (pbBindingsByIdentity base)))

runtimeBindingNameAvailable :: ProgramBase -> String -> Bool
runtimeBindingNameAvailable base name =
  Set.notMember name (programBindingRuntimeNames base)

bindingSelfReference :: BindingInfo -> Maybe IdDetails -> String -> Bool
bindingSelfReference binding mbIdentity name =
  symbolRefMatches (biIdentity binding) (biName binding) (backendVarSymbolIdentity mbIdentity) name

lookupNonLocalBindingInfo :: ProgramBase -> Maybe IdDetails -> Maybe BindingInfo
lookupNonLocalBindingInfo base mbIdentity =
  case (mbIdentity >>= lowerLocalKey, backendVarSymbolIdentity mbIdentity) of
    (Just _, _) ->
      Nothing
    (_, Just identity) ->
      Map.lookup identity (pbBindingsByIdentity base)
    _ ->
      Nothing

backendVarSymbolIdentity :: Maybe IdDetails -> Maybe SymbolIdentity
backendVarSymbolIdentity =
  (>>= idDetailsSymbolIdentity)

primitiveRuntimeName :: Maybe IdDetails -> String -> Maybe String
primitiveRuntimeName mbIdentity _name =
  case mbIdentity of
    Just (PrimitiveId ref) -> primitiveSymbolRuntimeName (primitiveRefSymbol ref)
    Just (TopLevelId symbol) -> primitiveSymbolRuntimeName symbol
    Just _ -> Nothing
    Nothing -> Nothing

primitiveSymbolRuntimeName :: SymbolIdentity -> Maybe String
primitiveSymbolRuntimeName =
  PrimitiveInventory.primitiveValueNameByIdentity

ioPrimitiveRuntimeName :: Maybe IdDetails -> String -> Maybe String
ioPrimitiveRuntimeName mbIdentity name =
  case primitiveRuntimeName mbIdentity name of
    Just primitiveName | Set.member primitiveName ioPrimitiveNames -> Just primitiveName
    _ -> Nothing

nativePrimitiveRuntimeName :: Maybe IdDetails -> String -> Maybe String
nativePrimitiveRuntimeName mbIdentity name =
  case primitiveRuntimeName mbIdentity name of
    Just primitiveName | Set.member primitiveName nativePrimitiveNames -> Just primitiveName
    _ -> Nothing

resolvedNonLocalReference :: Maybe IdDetails -> Bool
resolvedNonLocalReference mbIdentity =
  case backendVarSymbolIdentity mbIdentity of
    Just _ -> True
    Nothing -> False

constructorRuntimes :: IdentityGenerator -> BackendData -> (IdentityGenerator, [ConstructorRuntime])
constructorRuntimes generator dataDecl =
  mapAccumL constructorRuntime generator (zip [0 ..] (backendDataConstructors dataDecl))
  where
    constructorRuntime generator0 (tag, constructor) =
      let (key, generator1) = constructorValueKey generator0 constructor
       in ( generator1,
            ConstructorRuntime
              { crConstructor = constructor,
                crData = dataDecl,
                crTag = tag,
                crValueKey = key
              }
          )

    constructorValueKey generator0 constructor =
      case backendConstructorIdentity constructor of
        Just identity -> (constructorValueKeyFromIdentity identity, generator0)
        Nothing ->
          let (identity, generator1) = freshIdentity generator0
           in (constructorValueKeyFromGenerated identity (backendConstructorName constructor), generator1)

lookupConstructorRuntime :: ProgramBase -> Maybe SymbolIdentity -> String -> Maybe ConstructorRuntime
lookupConstructorRuntime base mbIdentity _name =
  case mbIdentity of
    Just identity -> Map.lookup identity (pbConstructorsByIdentity base)
    Nothing -> Nothing

dataRuntime :: IdentityGenerator -> BackendData -> (IdentityGenerator, DataRuntime)
dataRuntime generator dataDecl =
  ( generator',
    DataRuntime
      { drData = dataDecl,
        drConstructors = constructors
      }
  )
  where
    (generator', constructors) = constructorRuntimes generator dataDecl

functionFormFromExpr :: BackendExpr -> FunctionForm
functionFormFromExpr expr =
  FunctionForm
    { ffTypeBinders = typeBinders,
      ffParams = [(name, ty) | (_, name, ty) <- params],
      ffParamIdentities = [identity | (identity, _, _) <- params],
      ffEvidenceParams = Set.empty,
      ffBody = body,
      ffReturnType = backendExprType body
    }
  where
    (typeBinders, afterTypes) = collectTypeAbs expr
    (params, body) = collectLams afterTypes

functionFormFromExpected :: BackendType -> BackendExpr -> FunctionForm
functionFormFromExpected expectedTy expr =
  fst (functionFormFromExpectedWithGenerator generator expectedTy expr)
  where
    generator =
      identityGeneratorAfter
        (generatedIdentitiesInBackendTypes [expectedTy] ++ generatedIdentitiesInBackendExpr expr)

functionFormFromExpectedWithGenerator :: IdentityGenerator -> BackendType -> BackendExpr -> (FunctionForm, IdentityGenerator)
functionFormFromExpectedWithGenerator generator expectedTy expr =
  case freshenFunctionFormMissingTypeBinderIdentities generator (functionFormFromExpr expr) of
    (typedForm, generator0) ->
      case freshenFunctionFormMissingParamIdentities generator0 typedForm of
        (form0, generator')
          | let form = alignFunctionFormTypeBindersWithExpected expectedTy form0,
            Just (completed, generator'') <- completeAliasFunctionFormWithGenerator generator' form ->
              (completed, generator'')
          | let form = alignFunctionFormTypeBindersWithExpected expectedTy form0,
            not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              (form, generator')
          | otherwise ->
              case aliasFunctionFormWithGenerator generator' expectedTy expr of
                Just result -> result
                Nothing ->
                  case expectedNullaryReturnType expectedTy of
                    Just returnTy -> (form0 {ffReturnType = returnTy}, generator')
                    Nothing -> (form0, generator')

alignFunctionFormTypeBindersWithExpected :: BackendType -> FunctionForm -> FunctionForm
alignFunctionFormTypeBindersWithExpected expectedTy form
  | length expectedBinders == length formBinders,
    alphaEqBackendType expectedTy (functionFormType form) =
      form
        { ffTypeBinders = expectedBinders,
          ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
          ffBody = substituteExprTypesByKey substitution (ffBody form),
          ffReturnType = substituteTy (ffReturnType form)
        }
  | otherwise = form
  where
    (expectedBinders, _) = collectForallsType expectedTy
    formBinders = ffTypeBinders form
    substitution =
      Map.fromList
        [ (functionTypeBinderKey formBinder, functionTypeBinderVar expectedBinder)
        | (expectedBinder, formBinder) <- zip expectedBinders formBinders,
          functionTypeBinderKey expectedBinder /= functionTypeBinderKey formBinder
        ]
    substituteTy =
      substituteBackendTypesByKey substitution

freshenFunctionFormMissingTypeBinderIdentities :: IdentityGenerator -> FunctionForm -> (FunctionForm, IdentityGenerator)
freshenFunctionFormMissingTypeBinderIdentities generator form =
  ( form
      { ffTypeBinders = binders,
        ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
        ffBody = substituteExprTypesByKey substitution (ffBody form),
        ffReturnType = substituteTy (ffReturnType form)
      },
    generator'
  )
  where
    substituteTy =
      substituteBackendTypesByKey substitution

    (generator', binders, substitution) =
      foldl assignBinder (generator, [], Map.empty) (ffTypeBinders form)

    assignBinder (generator0, bindersAcc, substitutionAcc) binder =
      let name = backendTypeBinderName binder
          oldIdentity = backendTypeBinderIdentity binder
          oldKey = backendTypeSubstitutionKeyFor oldIdentity name
          bound' = fmap (substituteBackendTypesByKey substitutionAcc) (backendTypeBinderBound binder)
          (newIdentity, generator1) =
            case oldIdentity of
              Just identity -> (Just identity, generator0)
              Nothing ->
                let (unique, generatorNext) = freshIdentity generator0
                 in (Just (typeBinderIdentityFromUnique unique), generatorNext)
          binder' =
            BackendTypeBinderWithIdentity
              newIdentity
              name
              bound'
          replacement =
            BTVarWithIdentity newIdentity name
          substitutionAcc' =
            case newIdentity of
              Just identity
                | oldKey == backendTypeSubstitutionKeyFromIdentity identity -> substitutionAcc
              _ -> Map.insert oldKey replacement substitutionAcc
       in (generator1, bindersAcc ++ [binder'], substitutionAcc')

expectedNullaryReturnType :: BackendType -> Maybe BackendType
expectedNullaryReturnType ty =
  case collectForallsType ty of
    ([], afterForalls) ->
      case collectArrowsType afterForalls of
        ([], returnTy) -> Just returnTy
        _ -> Nothing
    _ -> Nothing

functionFormFromExpectedM :: BackendType -> BackendExpr -> LowerM FunctionForm
functionFormFromExpectedM expectedTy expr = do
  state0 <- get
  let (form, generator') =
        functionFormFromExpectedWithGenerator
          (fsIdentityGenerator state0)
          expectedTy
          expr
  put state0 {fsIdentityGenerator = generator'}
  pure form

freshenFunctionFormMissingParamIdentities :: IdentityGenerator -> FunctionForm -> (FunctionForm, IdentityGenerator)
freshenFunctionFormMissingParamIdentities generator form =
  ( form
      { ffParamIdentities = paramIdentities,
        ffBody = rewriteBackendVarsByName generatedIdentities (ffBody form)
      },
    generator'
  )
  where
    (generator', paramIdentities) =
      mapAccumL assignIdentity generator (zip (ffParamIdentities form ++ repeat Nothing) (ffParams form))
    generatedIdentities =
      generatedBackendTermEnv
        [ (name, Just identity)
        | ((originalIdentity, (name, _)), Just identity) <- zip (zip (ffParamIdentities form ++ repeat Nothing) (ffParams form)) paramIdentities,
          Nothing <- [originalIdentity]
        ]

    assignIdentity generator0 (Just identity, _) =
      (generator0, Just identity)
    assignIdentity generator0 (Nothing, (name, _)) =
      let (localRef, generator1) = freshLocalRef name generator0
       in (generator1, Just (LocalId localRef))

generatedBackendTermEnv :: [(String, Maybe IdDetails)] -> BackendTermEnv
generatedBackendTermEnv =
  foldl insertOne Map.empty
  where
    insertOne env (name, Just identity) =
      insertUniqueBackendTermIdentity name identity env
    insertOne env (_, Nothing) =
      env

rewriteBackendVarsByName :: BackendTermEnv -> BackendExpr -> BackendExpr
rewriteBackendVarsByName identities0 =
  go identities0
  where
    go identities =
      \case
        BackendVarWithIdentity ty Nothing name
          | Just (Just identity) <- Map.lookup name identities ->
              BackendVarWithIdentity ty (Just identity) name
        BackendVarWithIdentity ty mbIdentity name ->
          BackendVarWithIdentity ty mbIdentity name
        BackendLit ty lit ->
          BackendLit ty lit
        BackendLamWithIdentity resultTy identity name paramTy body ->
          BackendLamWithIdentity resultTy identity name paramTy (go (Map.delete name identities) body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go identities fun) (go identities arg)
        BackendLetWithIdentity resultTy identity name bindingTy rhs body ->
          BackendLetWithIdentity resultTy identity name bindingTy (go identities rhs) (go (Map.delete name identities) body)
        BackendTyAbsWithIdentity resultTy identity name mbBound body ->
          BackendTyAbsWithIdentity resultTy identity name mbBound (go identities body)
        BackendTyApp resultTy fun ty ->
          BackendTyApp resultTy (go identities fun) ty
        BackendConstructWithIdentity resultTy identity name args ->
          BackendConstructWithIdentity resultTy identity name (map (go identities) args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go identities scrutinee) (fmap (rewriteAlternative identities) alternatives)
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go identities payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go identities payload)
        BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
          BackendClosureWithParamIdentities resultTy entryIdentity entryName (map (rewriteCapture identities) captures) params (go (withoutClosureLocals captures params identities) body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall resultTy (go identities fun) (map (go identities) args)

    rewriteAlternative identities alternative =
      alternative {backendAltBody = go (withoutPatternBinders (backendAltPattern alternative) identities) (backendAltBody alternative)}

    rewriteCapture identities capture =
      capture {backendClosureCaptureExpr = go identities (backendClosureCaptureExpr capture)}

    withoutClosureLocals captures params =
      withoutNames (map backendClosureCaptureName captures ++ map backendClosureParamName params)

    withoutPatternBinders =
      withoutNames . map backendPatternBinderName . patternLocalBinders

    withoutNames names identities =
      foldr Map.delete identities names

    patternLocalBinders =
      \case
        BackendDefaultPattern -> []
        BackendConstructorPatternWithBinderIdentities _ _ binders -> binders

markFunctionFormEvidenceParams :: Set Int -> FunctionForm -> FunctionForm
markFunctionFormEvidenceParams evidenceIndices form =
  form
    { ffEvidenceParams =
        Set.filter (\index0 -> index0 >= 0 && index0 < length (ffParams form)) evidenceIndices
    }

indexed :: [a] -> [(Int, a)]
indexed =
  zip [0 :: Int ..]

functionFormParamIdentities :: FunctionForm -> [Maybe IdDetails]
functionFormParamIdentities form =
  ffParamIdentities form ++ repeat Nothing

functionFormParamTriples :: FunctionForm -> [(Maybe IdDetails, String, BackendType)]
functionFormParamTriples form =
  zipWith
    (\identity (name, ty) -> (identity, name, ty))
    (functionFormParamIdentities form)
    (ffParams form)

closureEntryParamIdentities :: ClosureEntry -> [Maybe IdDetails]
closureEntryParamIdentities entry =
  ceParamIdentities entry ++ repeat Nothing

closureEntryParamTriples :: ClosureEntry -> [(Maybe IdDetails, String, BackendType)]
closureEntryParamTriples entry =
  zipWith
    (\identity (name, ty) -> (identity, name, ty))
    (closureEntryParamIdentities entry)
    (ceParams entry)

functionTypeBinderKey :: BackendTypeBinder -> BackendTypeSubstitutionKey
functionTypeBinderKey binder =
  backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder)

functionTypeBinderVar :: BackendTypeBinder -> BackendType
functionTypeBinderVar binder =
  BTVarWithIdentity (backendTypeBinderIdentity binder) (backendTypeBinderName binder)

functionTypeBinderNames :: [BackendTypeBinder] -> [String]
functionTypeBinderNames =
  map backendTypeBinderName

substituteBackendTypeForBinderKey :: Maybe TypeBinderIdentity -> String -> BackendType -> BackendType -> BackendType
substituteBackendTypeForBinderKey identity name replacement =
  substituteBackendTypeForBinder identity name replacement

backendTyAbsIdentity :: BackendExpr -> Maybe TypeBinderIdentity
backendTyAbsIdentity =
  \case
    BackendTyAbsWithIdentity _ identity _ _ _ -> identity
    _ -> Nothing

deleteTypeBinderSubstitution :: Maybe TypeBinderIdentity -> String -> Map BackendTypeSubstitutionKey BackendType -> Map BackendTypeSubstitutionKey BackendType
deleteTypeBinderSubstitution identity name =
  Map.delete (backendTypeSubstitutionKeyFor identity name)

substituteFunctionFormTypes :: Map BackendTypeSubstitutionKey BackendType -> FunctionForm -> FunctionForm
substituteFunctionFormTypes substitution0 form =
  FunctionForm
    { ffTypeBinders = [binder {backendTypeBinderBound = fmap substituteTy (backendTypeBinderBound binder)} | binder <- ffTypeBinders form],
      ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
      ffParamIdentities = ffParamIdentities form,
      ffEvidenceParams = ffEvidenceParams form,
      ffBody = substituteExprTypesByKey substitution (ffBody form),
      ffReturnType = substituteTy (ffReturnType form)
    }
  where
    binderKeys = Set.fromList (map functionTypeBinderKey (ffTypeBinders form))
    substitution = Map.withoutKeys substitution0 binderKeys
    substituteTy = substituteBackendTypesByKey substitution

completeAliasFunctionFormWithGenerator :: IdentityGenerator -> FunctionForm -> Maybe (FunctionForm, IdentityGenerator)
completeAliasFunctionFormWithGenerator generator form
  | not (isAliasExpr (ffBody form)) = Nothing
  | null params = Nothing
  | otherwise = do
      let argNames = take (length params) ["__mlfp_alias_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]
          (generator', identities) = generatedLocalIdentities generator argNames
          args = zip3 identities argNames params
      body <- applyAliasArguments (ffBody form) (ffReturnType form) args
      pure
        ( form
            { ffParams = ffParams form ++ zip argNames params,
              ffParamIdentities = ffParamIdentities form ++ identities,
              ffBody = body,
              ffReturnType = returnTy
            },
          generator'
        )
  where
    (params, returnTy) = collectArrowsType (ffReturnType form)

aliasFunctionFormWithGenerator :: IdentityGenerator -> BackendType -> BackendExpr -> Maybe (FunctionForm, IdentityGenerator)
aliasFunctionFormWithGenerator generator expectedTy expr
  | not (isAliasExpr expr) = Nothing
  | null typeBinders && null params = Nothing
  | otherwise = do
      headExpr <- either (const Nothing) Just (applyTypeApplicationsToExpr "function alias" afterForalls expr typeArgs)
      let argNames = take (length params) ["__mlfp_alias_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]
          (generator', identities) = generatedLocalIdentities generator argNames
          args = zip3 identities argNames params
      body <- applyAliasArguments headExpr afterForalls args
      pure
        ( FunctionForm
            { ffTypeBinders = typeBinders,
              ffParams = zip argNames params,
              ffParamIdentities = identities,
              ffEvidenceParams = Set.empty,
              ffBody = body,
              ffReturnType = returnTy
            },
          generator'
        )
  where
    (typeBinders, afterForalls) = collectForallsType expectedTy
    (params, returnTy) = collectArrowsType afterForalls
    typeArgs = map functionTypeBinderVar typeBinders

generatedLocalIdentities :: IdentityGenerator -> [String] -> (IdentityGenerator, [Maybe IdDetails])
generatedLocalIdentities =
  mapAccumL
    ( \generator name ->
        let (localRef, generator') = freshLocalRef name generator
         in (generator', Just (LocalId localRef))
    )

isAliasExpr :: BackendExpr -> Bool
isAliasExpr =
  \case
    BackendVarWithIdentity {} -> True
    BackendTyApp _ fun _ -> isAliasExpr fun
    BackendApp _ fun arg -> isAliasExpr fun && isAliasArgument arg
    BackendLet _ _ _ rhs body -> isTransparentAliasLetRhs rhs && isAliasExpr body
    _ -> False

isAliasLetRhs :: BackendExpr -> Bool
isAliasLetRhs =
  \case
    BackendVarWithIdentity {} -> True
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

collectForallsType :: BackendType -> ([BackendTypeBinder], BackendType)
collectForallsType =
  \case
    BTForallWithIdentity identity name mbBound body ->
      let (binders, core) = collectForallsType body
       in (BackendTypeBinderWithIdentity identity name mbBound : binders, core)
    ty -> ([], ty)

functionFormType :: FunctionForm -> BackendType
functionFormType form =
  foldr
    (\binder body -> BTForallWithIdentity (backendTypeBinderIdentity binder) (backendTypeBinderName binder) (backendTypeBinderBound binder) body)
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
    { eeValuesByIdentity = Map.empty,
      eeLocalFunctionsByIdentity = Map.empty,
      eeActiveGlobalInlines = Set.empty
    }

lookupExprEnvValue :: Maybe IdDetails -> ExprEnv -> Maybe LowerValue
lookupExprEnvValue mbIdentity exprEnv =
  if resolvedNonLocalReference mbIdentity
    then Nothing
    else
      case mbIdentity >>= lowerLocalKey of
        Just key ->
          Map.lookup key (eeValuesByIdentity exprEnv)
        Nothing ->
          Nothing

lookupExprEnvLocalFunction :: Maybe IdDetails -> ExprEnv -> Maybe LocalFunction
lookupExprEnvLocalFunction mbIdentity exprEnv =
  if resolvedNonLocalReference mbIdentity
    then Nothing
    else
      case mbIdentity >>= lowerLocalKey of
        Just key ->
          Map.lookup key (eeLocalFunctionsByIdentity exprEnv)
        Nothing ->
          Nothing

bindExprEnvValue :: Maybe IdDetails -> LowerValue -> ExprEnv -> ExprEnv
bindExprEnvValue mbIdentity value exprEnv =
  exprEnv
    { eeValuesByIdentity = maybe id (`Map.insert` value) mbKey (eeValuesByIdentity exprEnv),
      eeLocalFunctionsByIdentity = maybe id Map.delete mbKey (eeLocalFunctionsByIdentity exprEnv)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

bindExprEnvLocalFunction :: Maybe IdDetails -> LocalFunction -> ExprEnv -> ExprEnv
bindExprEnvLocalFunction mbIdentity localFunction exprEnv =
  exprEnv
    { eeLocalFunctionsByIdentity = maybe id (`Map.insert` localFunction) mbKey (eeLocalFunctionsByIdentity exprEnv),
      eeValuesByIdentity = maybe id Map.delete mbKey (eeValuesByIdentity exprEnv)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

deleteExprEnvBinding :: Maybe IdDetails -> ExprEnv -> ExprEnv
deleteExprEnvBinding mbIdentity exprEnv =
  exprEnv
    { eeValuesByIdentity = maybe id Map.delete mbKey (eeValuesByIdentity exprEnv),
      eeLocalFunctionsByIdentity = maybe id Map.delete mbKey (eeLocalFunctionsByIdentity exprEnv)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

collectArrowsType :: BackendType -> ([BackendType], BackendType)
collectArrowsType =
  \case
    BTArrow paramTy resultTy ->
      let (params, returnTy) = collectArrowsType resultTy
       in (paramTy : params, returnTy)
    ty -> ([], ty)

applyAliasArguments :: BackendExpr -> BackendType -> [(Maybe IdDetails, String, BackendType)] -> Maybe BackendExpr
applyAliasArguments expr _ [] =
  Just expr
applyAliasArguments expr ty ((identity, name, paramTy) : rest) =
  case ty of
    BTArrow expectedParamTy resultTy
      | alphaEqBackendType expectedParamTy paramTy ->
          applyAliasArguments (BackendApp resultTy expr (BackendVarWithIdentity paramTy identity name)) resultTy rest
    _ ->
      Nothing

collectTypeAbs :: BackendExpr -> ([BackendTypeBinder], BackendExpr)
collectTypeAbs =
  \case
    BackendTyAbsWithIdentity _ identity name mbBound body ->
      let (params, core) = collectTypeAbs body
       in (BackendTypeBinderWithIdentity identity name mbBound : params, core)
    expr -> ([], expr)

collectLams :: BackendExpr -> ([(Maybe IdDetails, String, BackendType)], BackendExpr)
collectLams expr =
  let (params, core) = collectRawLams expr
      paramNames = Set.fromList [name | (identity, name, _) <- params, not (hasLocalIdentity identity)]
      reserved = freeBackendExprVars core `Set.difference` paramNames
      (params', renaming) = freshenLambdaParams reserved params
   in (params', renameBackendVars renaming core)

collectRawLams :: BackendExpr -> ([(Maybe IdDetails, String, BackendType)], BackendExpr)
collectRawLams =
  \case
    BackendLamWithIdentity _ mbIdentity name paramTy body ->
      let (params, core) = collectRawLams body
       in ((mbIdentity, name, paramTy) : params, core)
    expr -> ([], expr)

freshenLambdaParams :: Set String -> [(Maybe IdDetails, String, BackendType)] -> ([(Maybe IdDetails, String, BackendType)], Map String String)
freshenLambdaParams =
  go Map.empty
  where
    go renaming used =
      \case
        [] -> ([], renaming)
        (identity, name, ty) : rest ->
          let name' = freshNameLike name used
              used' = Set.insert name' used
              renaming' =
                if hasLocalIdentity identity
                  then renaming
                  else Map.insert name name' renaming
              (rest', finalRenaming) = go renaming' used' rest
           in ((identity, name', ty) : rest', finalRenaming)

hasLocalIdentity :: Maybe IdDetails -> Bool
hasLocalIdentity mbIdentity =
  case mbIdentity >>= lowerLocalKey of
    Just _ -> True
    Nothing -> False

renameBackendVars :: Map String String -> BackendExpr -> BackendExpr
renameBackendVars renaming0 =
  go renaming0
  where
    renameName renaming name =
      Map.findWithDefault name name renaming

    go renaming =
      \case
        BackendVarWithIdentity resultTy Nothing name ->
          BackendVarWithIdentity resultTy Nothing (renameName renaming name)
        BackendVarWithIdentity resultTy mbIdentity name ->
          BackendVarWithIdentity resultTy mbIdentity name
        BackendLit resultTy lit ->
          BackendLit resultTy lit
        BackendLamWithIdentity resultTy mbIdentity name paramTy body ->
          BackendLamWithIdentity resultTy mbIdentity name paramTy (go (withoutBinder (mbIdentity, name) renaming) body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go renaming fun) (go renaming arg)
        BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs body ->
          BackendLetWithIdentity resultTy mbIdentity name bindingTy (go renaming rhs) (go (withoutBinder (mbIdentity, name) renaming) body)
        BackendTyAbsWithIdentity resultTy identity name mbBound body ->
          BackendTyAbsWithIdentity resultTy identity name mbBound (go renaming body)
        BackendTyApp resultTy fun ty ->
          BackendTyApp resultTy (go renaming fun) ty
        BackendConstructWithIdentity resultTy mbIdentity name args ->
          BackendConstructWithIdentity resultTy mbIdentity name (map (go renaming) args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go renaming scrutinee) (fmap (renameAlternative renaming) alternatives)
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go renaming payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go renaming payload)
        BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
          BackendClosureWithParamIdentities
            resultTy
            entryIdentity
            entryName
            (map (renameCapture renaming) captures)
            params
            (go (withoutBinders (closureBinderRefs captures params) renaming) body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall resultTy (go renaming fun) (map (go renaming) args)

    renameAlternative renaming (BackendAlternative pattern0 body) =
      BackendAlternative pattern0 (go (withoutPatternBinders pattern0 renaming) body)

    renameCapture renaming capture =
      capture {backendClosureCaptureExpr = go renaming (backendClosureCaptureExpr capture)}

    withoutBinder (mbIdentity, name) renaming
      | hasLocalIdentity mbIdentity = renaming
      | otherwise = Map.delete name renaming

    withoutBinders refs renaming =
      foldr withoutBinder renaming refs

    closureBinderRefs captures params =
      [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
        ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]

    withoutPatternBinders pattern0 renaming =
      case pattern0 of
        BackendDefaultPattern ->
          renaming
        BackendConstructorPatternWithBinderIdentities _ _ binders ->
          withoutBinders [(backendPatternBinderIdentity binder, backendPatternBinderName binder) | binder <- binders] renaming

freeBackendExprVars :: BackendExpr -> Set String
freeBackendExprVars =
  go Set.empty
  where
    go bound =
      \case
        BackendVarWithIdentity _ mbIdentity _
          | Just _ <- backendVarSymbolIdentity mbIdentity -> Set.empty
          | hasLocalIdentity mbIdentity -> Set.empty
        BackendVarWithIdentity _ _ name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        BackendLit {} ->
          Set.empty
        BackendLamWithIdentity _ mbIdentity name _ body ->
          go (bindNameOnly mbIdentity name bound) body
        BackendApp _ fun arg ->
          go bound fun `Set.union` go bound arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body ->
          go bound rhs `Set.union` go (bindNameOnly mbIdentity name bound) body
        BackendTyAbs _ _ _ body ->
          go bound body
        BackendTyApp _ fun _ ->
          go bound fun
        BackendConstructWithIdentity _ _ _ args ->
          Set.unions (map (go bound) args)
        BackendCase _ scrutinee alternatives ->
          go bound scrutinee `Set.union` Set.unions (map (freeAlternative bound) (NE.toList alternatives))
        BackendRoll _ payload ->
          go bound payload
        BackendUnroll _ payload ->
          go bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          Set.unions (map (go bound . backendClosureCaptureExpr) captures)
            `Set.union` go (foldr (uncurry bindNameOnly) bound (closureRefs captures params)) body
        BackendClosureCall _ fun args ->
          go bound fun `Set.union` Set.unions (map (go bound) args)

    freeAlternative bound (BackendAlternative pattern0 body) =
      go (foldr (uncurry bindNameOnly) bound (patternBinderRefs pattern0)) body

    bindNameOnly mbIdentity name bound
      | hasLocalIdentity mbIdentity = bound
      | otherwise = Set.insert name bound

    closureRefs captures params =
      [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
        ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]

backendExprVarTypesFor :: Set TermBoundKey -> BackendExpr -> Map TermBoundKey BackendType
backendExprVarTypesFor targets =
  go Set.empty
  where
    go shadowed =
      \case
        BackendVarWithIdentity ty mbIdentity _name
          | Just _ <- backendVarSymbolIdentity mbIdentity -> Map.empty
          | Just key <- mbIdentity >>= lowerLocalKey ->
              let boundKey = TermBoundIdentity key
               in if Set.member boundKey targets && Set.notMember boundKey shadowed
                    then Map.singleton boundKey ty
                    else Map.empty
          | otherwise -> Map.empty
        BackendLit {} ->
          Map.empty
        BackendLamWithIdentity _ mbIdentity _name _ body ->
          go (Set.union (termBoundKeys mbIdentity) shadowed) body
        BackendApp _ fun arg ->
          go shadowed fun `Map.union` go shadowed arg
        BackendLetWithIdentity _ mbIdentity _name _ rhs body ->
          go shadowed rhs `Map.union` go (Set.union (termBoundKeys mbIdentity) shadowed) body
        BackendTyAbs _ _ _ body ->
          go shadowed body
        BackendTyApp _ fun _ ->
          go shadowed fun
        BackendConstructWithIdentity _ _ _ args ->
          Map.unions (map (go shadowed) args)
        BackendCase _ scrutinee alternatives ->
          go shadowed scrutinee `Map.union` Map.unions (map (goAlternative shadowed) (NE.toList alternatives))
        BackendRoll _ payload ->
          go shadowed payload
        BackendUnroll _ payload ->
          go shadowed payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          Map.unions (map (go shadowed . backendClosureCaptureExpr) captures)
            `Map.union` go (Set.union (termBoundKeyRefs (map fst closureRefs)) shadowed) body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          go shadowed fun `Map.union` Map.unions (map (go shadowed) args)

    goAlternative shadowed (BackendAlternative pattern0 body) =
      go (Set.union shadowed (patternTermBoundKeys pattern0)) body

lookupBindingRef :: ProgramBase -> BackendBindingRef -> Maybe BindingInfo
lookupBindingRef base ref =
  case Map.lookup ref (pbBindingsByRef base) of
    Just binding -> Just binding
    Nothing ->
      backendBindingRefIdentity ref >>= (`Map.lookup` pbBindingsByIdentity base)

reachableBindings :: ProgramBase -> BindingInfo -> Either BackendLLVMError [BindingInfo]
reachableBindings base mainBinding =
  Right orderedReachable
  where
    reachableRefs = reachableBindingRefs base mainBinding
    orderedReachable =
      [ binding
      | ref <- pbBindingOrder base,
        Just binding <- [lookupBindingRef base ref],
        bindingInfoRef binding `Set.member` reachableRefs
      ]

reachableBindingRefs :: ProgramBase -> BindingInfo -> Set BackendBindingRef
reachableBindingRefs base mainBinding =
  close (Set.singleton (bindingInfoRef mainBinding)) Set.empty
  where
    close pending seen =
      case Set.minView (pending `Set.difference` seen) of
        Nothing -> seen
        Just (ref, pendingRest) ->
          case lookupBindingRef base ref of
            Nothing -> close pendingRest seen
            Just binding ->
              close
                (pendingRest `Set.union` freeGlobalBindingRefs base binding)
                (Set.insert ref seen)

requireProgramMainBinding :: ProgramBase -> BackendProgram -> Either BackendLLVMError BindingInfo
requireProgramMainBinding base program =
  case backendProgramMainIdentity program of
    Just identity ->
      case Map.lookup identity (pbBindingsByIdentity base) of
        Just binding -> Right binding
        Nothing -> Left (BackendLLVMUnknownFunction (backendProgramMain program))
    Nothing ->
      Left (BackendLLVMUnknownFunction (backendProgramMain program))

requireSpecRequestBinding :: ProgramBase -> SpecRequest -> Either BackendLLVMError BindingInfo
requireSpecRequestBinding base request =
  case srBindingIdentity request of
    Just identity ->
      case Map.lookup identity (pbBindingsByIdentity base) of
        Just binding -> Right binding
        Nothing -> Left (BackendLLVMUnknownFunction (srBindingName request))
    Nothing ->
      Left (BackendLLVMUnknownFunction (srBindingName request))

specRequestForBinding :: BindingInfo -> [BackendType] -> SpecRequest
specRequestForBinding binding typeArgs =
  SpecRequest
    { srBindingIdentity = biIdentity binding,
      srBindingName = biName binding,
      srTypeArgs = typeArgs
    }

collectRequiredSpecializations :: IdentityGenerator -> ProgramBase -> [BindingInfo] -> Either BackendLLVMError (IdentityGenerator, [Specialization])
collectRequiredSpecializations generator0 base reachable =
  go generator0 Map.empty initialRequests
  where
    initialRequests =
      concatMap
        (collectSpecializationRequestsInForm base Map.empty . biForm)
        (filter (null . ffTypeBinders . biForm) reachable)

    go generator seen [] =
      Right (generator, map snd (sortOn fst (Map.toList seen)))
    go generator seen (request : rest)
      | Map.member key seen = go generator seen rest
      | otherwise = do
          binding <- requireSpecRequestBinding base request
          form <- instantiateFunctionForm ("specialization " ++ srBindingName request) (biForm binding) (srTypeArgs request) []
          let functionName = specializedFunctionName request
              (bindingIdentity, generator') = freshIdentity generator
              spec =
                Specialization
                  { spRequest = request,
                    spBindingRef = backendBindingRefFromGenerated bindingIdentity functionName,
                    spFunctionName = functionName,
                    spForm = form
                  }
              nestedRequests = collectSpecializationRequestsInForm base Map.empty form
          go generator' (Map.insert key spec seen) (rest ++ nestedRequests)
      where
        key = specializationKey request

collectEvidenceWrappers :: IdentityGenerator -> ProgramBase -> [BindingInfo] -> [Specialization] -> (IdentityGenerator, [Wrapper])
collectEvidenceWrappers generator base reachable specializations =
  mapAccumL assignName generator (zip [(0 :: Int) ..] uniqueRequests)
  where
    requests =
      concatMap (collectEvidenceWrappersInForm base Map.empty Set.empty . biForm) monomorphicReachable
        ++ concatMap (collectEvidenceWrappersInForm base Map.empty Set.empty . qualifiedSpecializationForm) specializations
    monomorphicReachable =
      filter (null . ffTypeBinders . biForm) reachable
    uniqueRequests =
      map snd (Map.toAscList (Map.fromList [(wrapperKey' expected expr, (expected, expr)) | (expected, expr) <- requests]))
    assignName generator0 (index0, (expected, expr)) =
      let functionName = "__mlfp_evidence_wrapper$" ++ show index0
          (bindingIdentity, generator1) = freshIdentity generator0
          (generator', identities) = generatedLocalIdentities generator1 (wrapperParamNames evidenceWrapperArgPrefix expected)
       in ( generator',
            Wrapper
              { wrapperKind = EvidenceWrapperKind,
                wrapperBindingRef = backendBindingRefFromGenerated bindingIdentity functionName,
                wrapperKey = wrapperKey' expected expr,
                wrapperFunctionName = functionName,
                wrapperExpectedType = expected,
                wrapperExpr = expr,
                wrapperParamIdentities = identities
              }
          )

collectFunctionWrappers :: IdentityGenerator -> ProgramBase -> [BindingInfo] -> [Specialization] -> (IdentityGenerator, [Wrapper])
collectFunctionWrappers generator base reachable specializations =
  mapAccumL assignName generator (zip [(0 :: Int) ..] uniqueRequests)
  where
    requests =
      concatMap (collectFunctionWrappersInForm base Map.empty Set.empty . biForm) monomorphicReachable
        ++ concatMap (collectFunctionWrappersInForm base Map.empty Set.empty . qualifiedSpecializationForm) specializations
    monomorphicReachable =
      filter (null . ffTypeBinders . biForm) reachable
    uniqueRequests =
      map snd (Map.toAscList (Map.fromList [(wrapperKey' expected expr, (expected, expr)) | (expected, expr) <- requests]))
    assignName generator0 (index0, (expected, expr)) =
      let functionName = "__mlfp_function_wrapper$" ++ show index0
          (bindingIdentity, generator1) = freshIdentity generator0
          (generator', identities) = generatedLocalIdentities generator1 (wrapperParamNames functionWrapperArgPrefix expected)
       in ( generator',
            Wrapper
              { wrapperKind = FunctionWrapperKind,
                wrapperBindingRef = backendBindingRefFromGenerated bindingIdentity functionName,
                wrapperKey = wrapperKey' expected expr,
                wrapperFunctionName = functionName,
                wrapperExpectedType = expected,
                wrapperExpr = expr,
                wrapperParamIdentities = identities
              }
          )

data ReferencedFunction
  = ReferencedBinding BackendBindingRef
  | ReferencedGeneratedBinding BackendBindingRef
  deriving (Eq, Ord, Show)

data ReferencedFunctions = ReferencedFunctions
  { rfBindings :: Set BackendBindingRef,
    rfGeneratedBindings :: Set BackendBindingRef
  }

collectReferencedFunctions :: ProgramBase -> [BindingInfo] -> [Specialization] -> [Wrapper] -> [Wrapper] -> ReferencedFunctions
collectReferencedFunctions base reachable specializations evidenceWrappers functionWrappers =
  ReferencedFunctions
    { rfBindings =
        Set.fromList
          [ binding
          | ReferencedBinding binding <- refs
          ],
      rfGeneratedBindings =
        Set.fromList
          [ binding
          | ReferencedGeneratedBinding binding <- refs
          ]
    }
  where
    specializationsByKey =
      Map.fromList [(specializationKey (spRequest spec), spec) | spec <- specializations]
    refs =
      concatMap Set.toList $
        map (collectReferencedFunctionsInForm base specializationsByKey Map.empty Set.empty . biForm) reachable
          ++ map (collectReferencedFunctionsInForm base specializationsByKey Map.empty Set.empty . spForm) specializations
          ++ map (collectReferencedFunctionsInForm base specializationsByKey Map.empty Set.empty . evidenceWrapperForm) evidenceWrappers
          ++ map (collectReferencedFunctionsInForm base specializationsByKey Map.empty Set.empty . functionWrapperForm) functionWrappers

data LocalFunctionFormEntry = LocalFunctionFormEntry
  { lffeName :: String,
    lffeForm :: FunctionForm
  }

data LocalFunctionForms = LocalFunctionForms
  { lffByIdentity :: Map LowerLocalKey LocalFunctionFormEntry
  }

data LocalStoredFunction = LocalStoredFunction
  { lsfName :: String,
    lsfForm :: FunctionForm,
    lsfSourceExpr :: BackendExpr
  }

data TermBoundKey
  = TermBoundIdentity LowerLocalKey
  deriving (Eq, Ord)

data LocalStoredFunctions = LocalStoredFunctions
  { lsfsByIdentity :: Map LowerLocalKey LocalStoredFunction
  }

emptyLocalFunctionForms :: LocalFunctionForms
emptyLocalFunctionForms =
  LocalFunctionForms Map.empty

emptyLocalStoredFunctions :: LocalStoredFunctions
emptyLocalStoredFunctions =
  LocalStoredFunctions Map.empty

lookupLocalStoredFunction :: Maybe IdDetails -> LocalStoredFunctions -> Maybe LocalStoredFunction
lookupLocalStoredFunction mbIdentity functions =
  if resolvedNonLocalReference mbIdentity
    then Nothing
    else
      case mbIdentity >>= lowerLocalKey of
        Just key ->
          Map.lookup key (lsfsByIdentity functions)
        Nothing ->
          Nothing

bindLocalStoredFunction :: Maybe IdDetails -> String -> FunctionForm -> BackendExpr -> LocalStoredFunctions -> LocalStoredFunctions
bindLocalStoredFunction mbIdentity name form sourceExpr functions =
  functions
    { lsfsByIdentity = maybe id (`Map.insert` localFunction) mbKey (lsfsByIdentity functions)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey
    localFunction =
      LocalStoredFunction
        { lsfName = name,
          lsfForm = form,
          lsfSourceExpr = sourceExpr
        }

deleteLocalStoredFunction :: Maybe IdDetails -> LocalStoredFunctions -> LocalStoredFunctions
deleteLocalStoredFunction mbIdentity functions =
  functions
    { lsfsByIdentity = maybe id Map.delete mbKey (lsfsByIdentity functions)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

shadowLocalStoredFunctions :: [Maybe IdDetails] -> LocalStoredFunctions -> LocalStoredFunctions
shadowLocalStoredFunctions binders functions =
  foldl' (flip deleteLocalStoredFunction) functions binders

localStoredFunctionKey :: Maybe IdDetails -> Maybe LowerLocalKey
localStoredFunctionKey =
  (>>= lowerLocalKey)

termBoundKeys :: Maybe IdDetails -> Set TermBoundKey
termBoundKeys mbIdentity =
  case mbIdentity >>= lowerLocalKey of
    Just key -> Set.singleton (TermBoundIdentity key)
    Nothing -> Set.empty

termReferenceKeys :: Maybe IdDetails -> Set TermBoundKey
termReferenceKeys mbIdentity
  | Just _ <- backendVarSymbolIdentity mbIdentity = Set.empty
  | otherwise = termBoundKeys mbIdentity

termBoundKeyRefs :: [Maybe IdDetails] -> Set TermBoundKey
termBoundKeyRefs =
  Set.unions . map termBoundKeys

patternTermBoundKeys :: BackendPattern -> Set TermBoundKey
patternTermBoundKeys =
  termBoundKeyRefs . map fst . patternBinderRefs

type LocalConstructedValues = Map TermBoundKey ConstructedValue

lookupLocalConstructedValue :: Maybe IdDetails -> LocalConstructedValues -> Maybe ConstructedValue
lookupLocalConstructedValue mbIdentity constructedValues =
  if resolvedNonLocalReference mbIdentity
    then Nothing
    else
      case mbIdentity >>= lowerLocalKey of
        Just key ->
          Map.lookup (TermBoundIdentity key) constructedValues
        Nothing ->
          Nothing

bindLocalConstructedValue :: Maybe IdDetails -> ConstructedValue -> LocalConstructedValues -> LocalConstructedValues
bindLocalConstructedValue mbIdentity constructed constructedValues =
  case termStoreKey mbIdentity of
    Just key ->
      Map.insert key constructed (deleteLocalConstructedValue mbIdentity constructedValues)
    Nothing ->
      deleteLocalConstructedValue mbIdentity constructedValues

deleteLocalConstructedValue :: Maybe IdDetails -> LocalConstructedValues -> LocalConstructedValues
deleteLocalConstructedValue mbIdentity constructedValues =
  constructedValues `Map.withoutKeys` (termBoundKeys mbIdentity)

termStoreKey :: Maybe IdDetails -> Maybe TermBoundKey
termStoreKey mbIdentity =
  TermBoundIdentity <$> (mbIdentity >>= lowerLocalKey)

lookupLocalFunctionFormEntry :: Maybe IdDetails -> LocalFunctionForms -> Maybe LocalFunctionFormEntry
lookupLocalFunctionFormEntry mbIdentity forms =
  if resolvedNonLocalReference mbIdentity
    then Nothing
    else
      case mbIdentity >>= lowerLocalKey of
        Just key ->
          Map.lookup key (lffByIdentity forms)
        Nothing ->
          Nothing

lookupLocalFunctionForm :: Maybe IdDetails -> LocalFunctionForms -> Maybe FunctionForm
lookupLocalFunctionForm mbIdentity forms =
  lffeForm <$> lookupLocalFunctionFormEntry mbIdentity forms

bindLocalFunctionForm :: Maybe IdDetails -> String -> FunctionForm -> LocalFunctionForms -> LocalFunctionForms
bindLocalFunctionForm mbIdentity name form forms =
  forms
    { lffByIdentity = maybe id (`Map.insert` entry) mbKey (lffByIdentity forms)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey
    entry = LocalFunctionFormEntry name form

deleteLocalFunctionForm :: Maybe IdDetails -> LocalFunctionForms -> LocalFunctionForms
deleteLocalFunctionForm mbIdentity forms =
  forms
    { lffByIdentity = maybe id Map.delete mbKey (lffByIdentity forms)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

shadowLocalFunctionForms :: [Maybe IdDetails] -> LocalFunctionForms -> LocalFunctionForms
shadowLocalFunctionForms binders forms =
  foldl' (flip deleteLocalFunctionForm) forms binders

data LocalValueKinds = LocalValueKinds
  { lvkByIdentity :: Map LowerLocalKey LowerValueKind
  }

emptyLocalValueKinds :: LocalValueKinds
emptyLocalValueKinds =
  LocalValueKinds Map.empty

lookupLocalValueKind :: Maybe IdDetails -> LocalValueKinds -> Maybe LowerValueKind
lookupLocalValueKind mbIdentity kinds =
  if resolvedNonLocalReference mbIdentity
    then Nothing
    else
      case mbIdentity >>= lowerLocalKey of
        Just key ->
          Map.lookup key (lvkByIdentity kinds)
        Nothing ->
          Nothing

bindLocalValueKind :: Maybe IdDetails -> LowerValueKind -> LocalValueKinds -> LocalValueKinds
bindLocalValueKind mbIdentity kind kinds =
  kinds
    { lvkByIdentity = maybe id (`Map.insert` kind) mbKey (lvkByIdentity kinds)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

deleteLocalValueKind :: Maybe IdDetails -> LocalValueKinds -> LocalValueKinds
deleteLocalValueKind mbIdentity kinds =
  kinds
    { lvkByIdentity = maybe id Map.delete mbKey (lvkByIdentity kinds)
    }
  where
    mbKey = mbIdentity >>= lowerLocalKey

shadowLocalValueKinds :: [Maybe IdDetails] -> LocalValueKinds -> LocalValueKinds
shadowLocalValueKinds binders kinds =
  foldl' (flip deleteLocalValueKind) kinds binders

unionLocalValueKinds :: LocalValueKinds -> LocalValueKinds -> LocalValueKinds
unionLocalValueKinds left right =
  LocalValueKinds
    { lvkByIdentity = Map.union (lvkByIdentity left) (lvkByIdentity right)
    }

exprEnvLocalValueKinds :: ExprEnv -> LocalValueKinds
exprEnvLocalValueKinds exprEnv =
  LocalValueKinds
    { lvkByIdentity = Map.map lvValueKind (eeValuesByIdentity exprEnv)
    }

collectReferencedFunctionsInForm :: ProgramBase -> Map String Specialization -> Map BackendTypeSubstitutionKey BackendType -> Set TermBoundKey -> FunctionForm -> Set ReferencedFunction
collectReferencedFunctionsInForm base specializationsByKey substitution bound form =
  collectReferencedFunctionsInFormWithLocals base specializationsByKey substitution emptyLocalFunctionForms bound form

collectReferencedFunctionsInFormWithLocals :: ProgramBase -> Map String Specialization -> Map BackendTypeSubstitutionKey BackendType -> LocalFunctionForms -> Set TermBoundKey -> FunctionForm -> Set ReferencedFunction
collectReferencedFunctionsInFormWithLocals base specializationsByKey substitution localForms bound form =
  let paramRefs = [(mbIdentity, name) | (mbIdentity, name, _) <- functionFormParamTriples form]
   in collectReferencedFunctionsInExpr
        base
        specializationsByKey
        substitution
        (shadowLocalFunctionForms (map fst paramRefs) localForms)
        (Set.union (termBoundKeyRefs (map fst paramRefs)) bound)
        (ffBody form)

collectReferencedFunctionsInExpr :: ProgramBase -> Map String Specialization -> Map BackendTypeSubstitutionKey BackendType -> LocalFunctionForms -> Set TermBoundKey -> BackendExpr -> Set ReferencedFunction
collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound expr =
  referencedHere `Set.union` childReferences
  where
    referencedHere =
      case collectCall expr of
        Just (callee, typeArgs, args) ->
          let typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs
              args' = map (substituteExprTypesByKey substitution) args
           in case instantiateFunctionFormWithTypeArgs "referenced function argument" (callableForm callee) typeArgs' args' of
                Right (_, form) ->
                  localCallReferences callee typeArgs' args'
                    `Set.union` Set.fromList
                      [ functionRef
                      | ((_, paramTy), arg) <- zip (ffParams form) args',
                        isFunctionLikeBackendType paramTy,
                        Just functionRef <- [referencedFunctionArgument arg]
                      ]
                Left _ -> Set.empty
        Nothing ->
          localTypeApplicationReferences

    callableForm callee =
      case callee of
        BackendVarWithIdentity calleeTy mbIdentity _name ->
          case lookupLocalFunctionForm mbIdentity localForms of
            Just form -> form
            Nothing -> functionFormFromType calleeTy
        _ -> functionFormFromExpr callee

    referencedFunctionArgument arg =
      case collectTyApps arg of
        (BackendVarWithIdentity _ mbIdentity name, typeArgs) ->
          referencedFunctionByName Set.empty mbIdentity name typeArgs
        _ -> Nothing

    referencedFunctionByName seen mbIdentity name typeArgs
      | Just key <- mbKey,
        Set.member key seen =
          Nothing
      | Just key <- mbKey,
        Just form <- lookupLocalFunctionForm mbIdentity localForms =
          case instantiateFunctionFormWithTypeArgs ("referenced local function argument " ++ name) form typeArgs [] of
            Right (_, instantiated) ->
              case etaAliasTarget instantiated of
                Just (targetIdentity, targetName, targetTypeArgs) ->
                  referencedFunctionByName (Set.insert key seen) targetIdentity targetName targetTypeArgs
                Nothing -> Nothing
            Left _ -> Nothing
      | Just binding <- lookupSpecializationBinding base mbIdentity =
          case instantiateFunctionFormWithTypeArgs ("referenced function argument " ++ name) (biForm binding) typeArgs [] of
            Right (resolvedTypeArgs, _)
              | null (ffTypeBinders (biForm binding)) -> Just (ReferencedBinding (bindingInfoRef binding))
              | otherwise ->
                  ReferencedGeneratedBinding . spBindingRef
                    <$> Map.lookup (specializationKey (specRequestForBinding binding resolvedTypeArgs)) specializationsByKey
            Left _ -> Nothing
      | otherwise = Nothing
      where
        mbKey = localStoredFunctionKey mbIdentity

    localCallReferences callee typeArgs args =
      case callee of
        BackendVarWithIdentity _ mbIdentity name
          | Just form <- lookupLocalFunctionForm mbIdentity localForms ->
              collectInstantiatedLocalReferences name form typeArgs args
        _ -> Set.empty

    localTypeApplicationReferences =
      case collectTyApps expr of
        (BackendVarWithIdentity _ mbIdentity name, typeArgs)
          | Just form <- lookupLocalFunctionForm mbIdentity localForms ->
              collectInstantiatedLocalReferences name form (map (substituteBackendTypesByKey substitution) typeArgs) []
        _ -> Set.empty

    collectInstantiatedLocalReferences name form typeArgs args =
      case instantiateFunctionFormWithTypeArgs ("referenced local function " ++ name) form typeArgs args of
        Right (_, instantiated) ->
          collectReferencedFunctionsInFormWithLocals base specializationsByKey Map.empty localForms bound instantiated
        Left _ -> Set.empty

    childReferences =
      case expr of
        BackendVarWithIdentity {} -> Set.empty
        BackendLit {} -> Set.empty
        BackendLamWithIdentity _ mbIdentity _name _ body ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution (deleteLocalFunctionForm mbIdentity localForms) (Set.union (termBoundKeys mbIdentity) bound) body
        BackendApp _ fun arg ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound fun
            `Set.union` collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound arg
        BackendLetWithIdentity _ mbIdentity name bindingTy rhs body ->
          collectLetRhsReferences bindingTy rhs
            `Set.union` collectReferencedFunctionsInExpr base specializationsByKey substitution (collectLetLocalForms mbIdentity name bindingTy rhs) (Set.union (termBoundKeys mbIdentity) bound) body
        BackendTyAbs _ name _ body ->
          collectReferencedFunctionsInExpr base specializationsByKey (deleteTypeBinderSubstitution (backendTyAbsIdentity expr) name substitution) localForms bound body
        BackendTyApp _ fun _ ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound fun
        BackendConstructWithIdentity _ _ _ args ->
          Set.unions (map (collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound) args)
        BackendCase _ scrutinee alternatives ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound scrutinee
            `Set.union` Set.unions (map collectAlternativeReferences (NE.toList alternatives))
        BackendRoll _ payload ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound payload
        BackendUnroll _ payload ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          Set.unions (map (collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound . backendClosureCaptureExpr) captures)
            `Set.union` collectReferencedFunctionsInExpr
              base
              specializationsByKey
              substitution
              (shadowLocalFunctionForms (map fst closureRefs) localForms)
              (Set.union (termBoundKeyRefs (map fst closureRefs)) bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
        BackendClosure _ _ captures params body ->
          Set.unions (map (collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound . backendClosureCaptureExpr) captures)
            `Set.union` collectReferencedFunctionsInExpr
              base
              specializationsByKey
              substitution
              (shadowLocalFunctionForms (map fst closureRefs) localForms)
              (Set.union (termBoundKeyRefs (map fst closureRefs)) bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(Nothing, name) | (name, _) <- params]
        BackendClosureCall _ fun args ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound fun
            `Set.union` Set.unions (map (collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound) args)

    collectLetRhsReferences bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form0
          | not (null (ffTypeBinders form0)) || not (null (ffParams form0)) ->
              let form = substituteFunctionFormTypes substitution form0
               in if null (ffTypeBinders form)
                    then collectReferencedFunctionsInFormWithLocals base specializationsByKey Map.empty localForms bound form
                    else Set.empty
        _ ->
          collectReferencedFunctionsInExpr base specializationsByKey substitution localForms bound rhs

    collectLetLocalForms mbIdentity name bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              bindLocalFunctionForm mbIdentity name (substituteFunctionFormTypes substitution form) localForms
        _ ->
          deleteLocalFunctionForm mbIdentity localForms

    collectAlternativeReferences alternative =
      let binderRefs = patternBinderRefs (backendAltPattern alternative)
       in
      collectReferencedFunctionsInExpr
        base
        specializationsByKey
        substitution
        (shadowLocalFunctionForms (map fst binderRefs) localForms)
        (Set.union (patternTermBoundKeys (backendAltPattern alternative)) bound)
        (backendAltBody alternative)



collectEvidenceWrappersInForm :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> Set TermBoundKey -> FunctionForm -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInForm base substitution bound form =
  collectEvidenceWrappersInFormWithLocals base substitution emptyLocalFunctionForms bound form

collectEvidenceWrappersInFormWithLocals :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> LocalFunctionForms -> Set TermBoundKey -> FunctionForm -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInFormWithLocals base substitution localForms bound form =
  let paramRefs = [(mbIdentity, name) | (mbIdentity, name, _) <- functionFormParamTriples form]
   in collectEvidenceWrappersInExpr
        base
        substitution
        (shadowLocalFunctionForms (map fst paramRefs) localForms)
        (Set.union (termBoundKeyRefs (map fst paramRefs)) bound)
        (ffBody form)

collectEvidenceWrappersInExpr :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> LocalFunctionForms -> Set TermBoundKey -> BackendExpr -> [(BackendType, BackendExpr)]
collectEvidenceWrappersInExpr base substitution localForms bound expr =
  wrappersHere ++ childWrappers
  where
    wrappersHere =
      case collectCall expr of
        Just (BackendVarWithIdentity _ mbIdentity name, typeArgs, args)
          | Just form <- lookupLocalFunctionForm mbIdentity localForms ->
              let typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs
                  args' = map (substituteExprTypesByKey substitution) args
               in collectInstantiatedLocalWrappers name form typeArgs' args'
          | Just binding <- lookupSpecializationBinding base mbIdentity ->
              let typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs
                  args' = map (substituteExprTypesByKey substitution) args
               in case instantiateFunctionFormWithTypeArgs ("evidence wrapper request " ++ name) (biForm binding) typeArgs' args' of
                    Right (_, form) -> evidenceArgumentWrappers form args'
                    Left _ -> []
        _ -> localTypeApplicationWrappers

    childWrappers =
      case expr of
        BackendVarWithIdentity {} -> []
        BackendLit {} -> []
        BackendLamWithIdentity _ mbIdentity _name _ body ->
          collectEvidenceWrappersInExpr base substitution (deleteLocalFunctionForm mbIdentity localForms) (Set.union (termBoundKeys mbIdentity) bound) body
        BackendApp _ fun arg ->
          collectEvidenceWrappersInExpr base substitution localForms bound fun
            ++ collectEvidenceWrappersInExpr base substitution localForms bound arg
        BackendLetWithIdentity _ mbIdentity name bindingTy rhs body ->
          collectLetRhsWrappers bindingTy rhs
            ++ collectEvidenceWrappersInExpr base substitution (collectLetLocalForms mbIdentity name bindingTy rhs) (Set.union (termBoundKeys mbIdentity) bound) body
        BackendTyAbs _ name _ body ->
          collectEvidenceWrappersInExpr base (deleteTypeBinderSubstitution (backendTyAbsIdentity expr) name substitution) localForms bound body
        BackendTyApp _ fun _ ->
          collectEvidenceWrappersInExpr base substitution localForms bound fun
        BackendConstructWithIdentity _ _ _ args ->
          concatMap (collectEvidenceWrappersInExpr base substitution localForms bound) args
        BackendCase _ scrutinee alternatives ->
          collectEvidenceWrappersInExpr base substitution localForms bound scrutinee
            ++ concatMap collectAlternativeWrappers (NE.toList alternatives)
        BackendRoll _ payload ->
          collectEvidenceWrappersInExpr base substitution localForms bound payload
        BackendUnroll _ payload ->
          collectEvidenceWrappersInExpr base substitution localForms bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          concatMap (collectEvidenceWrappersInExpr base substitution localForms bound . backendClosureCaptureExpr) captures
            ++ collectEvidenceWrappersInExpr
              base
              substitution
              (shadowLocalFunctionForms (map fst closureRefs) localForms)
              (Set.union closureBoundKeys bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
            closureBoundKeys = termBoundKeyRefs (map fst closureRefs)
        BackendClosure _ _ captures params body ->
          concatMap (collectEvidenceWrappersInExpr base substitution localForms bound . backendClosureCaptureExpr) captures
            ++ collectEvidenceWrappersInExpr
              base
              substitution
              (shadowLocalFunctionForms (map fst closureRefs) localForms)
              (Set.union closureBoundKeys bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(Nothing, name) | (name, _) <- params]
            closureBoundKeys = termBoundKeyRefs (map fst closureRefs)
        BackendClosureCall _ fun args ->
          collectEvidenceWrappersInExpr base substitution localForms bound fun
            ++ concatMap (collectEvidenceWrappersInExpr base substitution localForms bound) args

    evidenceArgumentWrappers form args =
      [ (paramTy, arg)
      | (index0, ((_, paramTy), arg)) <- indexed (zip (ffParams form) args),
        isEvidenceArgument (ffEvidenceParams form) index0 paramTy,
        not (isSimpleFunctionReference arg),
        evidenceWrapperArgumentClosed bound arg
      ]

    localTypeApplicationWrappers =
      case collectTyApps expr of
        (BackendVarWithIdentity _ mbIdentity name, typeArgs)
          | Just form <- lookupLocalFunctionForm mbIdentity localForms ->
              collectInstantiatedLocalWrappers name form (map (substituteBackendTypesByKey substitution) typeArgs) []
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

    collectLetLocalForms mbIdentity name bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              bindLocalFunctionForm mbIdentity name (substituteFunctionFormTypes substitution form) localForms
        _ ->
          deleteLocalFunctionForm mbIdentity localForms

    collectAlternativeWrappers alternative =
      let binderRefs = patternBinderRefs (backendAltPattern alternative)
       in
      collectEvidenceWrappersInExpr
        base
        substitution
        (shadowLocalFunctionForms (map fst binderRefs) localForms)
        (Set.union (patternTermBoundKeys (backendAltPattern alternative)) bound)
        (backendAltBody alternative)



wrapperKey' :: BackendType -> BackendExpr -> String
wrapperKey' expected expr =
  backendTypeKey expected ++ "\0" ++ canonicalBackendExprKey expr

canonicalBackendExprKey :: BackendExpr -> String
canonicalBackendExprKey =
  \case
    BackendVarWithIdentity ty identity name ->
      "var(" ++ canonicalBackendTypeKey ty ++ "," ++ canonicalTermRefKey identity name ++ ")"
    BackendLit ty lit ->
      "lit(" ++ canonicalBackendTypeKey ty ++ "," ++ show lit ++ ")"
    BackendLamWithIdentity resultTy identity name paramTy body ->
      "lam(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalTermRefKey identity name ++ "," ++ canonicalBackendTypeKey paramTy ++ "," ++ canonicalBackendExprKey body ++ ")"
    BackendApp resultTy fun arg ->
      "app(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalBackendExprKey fun ++ "," ++ canonicalBackendExprKey arg ++ ")"
    BackendLetWithIdentity resultTy identity name bindingTy rhs body ->
      "let(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalTermRefKey identity name ++ "," ++ canonicalBackendTypeKey bindingTy ++ "," ++ canonicalBackendExprKey rhs ++ "," ++ canonicalBackendExprKey body ++ ")"
    BackendTyAbsWithIdentity resultTy identity name mbBound body ->
      "tyabs(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalTypeBinderKey identity name ++ "," ++ maybe "_" canonicalBackendTypeKey mbBound ++ "," ++ canonicalBackendExprKey body ++ ")"
    BackendTyApp resultTy fun ty ->
      "tyapp(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalBackendExprKey fun ++ "," ++ canonicalBackendTypeKey ty ++ ")"
    BackendRoll resultTy payload ->
      "roll(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalBackendExprKey payload ++ ")"
    BackendUnroll resultTy payload ->
      "unroll(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalBackendExprKey payload ++ ")"
    BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
      "closure(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalClosureEntryKey entryIdentity entryName ++ "," ++ canonicalListKey (map canonicalBackendClosureCaptureKey captures) ++ "," ++ canonicalListKey (map canonicalBackendClosureParamKey params) ++ "," ++ canonicalBackendExprKey body ++ ")"
    BackendClosureCall resultTy fun args ->
      "closurecall(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalBackendExprKey fun ++ "," ++ canonicalListKey (map canonicalBackendExprKey args) ++ ")"
    BackendConstructWithIdentity resultTy identity name args ->
      "construct(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalSymbolRefKey identity name ++ "," ++ canonicalListKey (map canonicalBackendExprKey args) ++ ")"
    BackendCase resultTy scrutinee alternatives ->
      "case(" ++ canonicalBackendTypeKey resultTy ++ "," ++ canonicalBackendExprKey scrutinee ++ "," ++ canonicalListKey (map canonicalBackendAlternativeKey (NE.toList alternatives)) ++ ")"

canonicalBackendClosureCaptureKey :: BackendClosureCapture -> String
canonicalBackendClosureCaptureKey capture =
  "capture("
    ++ canonicalTermRefKey (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture)
    ++ ","
    ++ canonicalBackendTypeKey (backendClosureCaptureType capture)
    ++ ","
    ++ canonicalBackendExprKey (backendClosureCaptureExpr capture)
    ++ ")"

canonicalBackendClosureParamKey :: BackendClosureParam -> String
canonicalBackendClosureParamKey param =
  "param("
    ++ canonicalTermRefKey (backendClosureParamIdentity param) (backendClosureParamName param)
    ++ ","
    ++ canonicalBackendTypeKey (backendClosureParamType param)
    ++ ")"

canonicalBackendAlternativeKey :: BackendAlternative -> String
canonicalBackendAlternativeKey (BackendAlternative pattern0 body) =
  "alt(" ++ canonicalBackendPatternKey pattern0 ++ "," ++ canonicalBackendExprKey body ++ ")"

canonicalBackendPatternKey :: BackendPattern -> String
canonicalBackendPatternKey =
  \case
    BackendDefaultPattern ->
      "default"
    BackendConstructorPatternWithBinderIdentities identity name binders ->
      "ctorpat(" ++ canonicalSymbolRefKey identity name ++ "," ++ canonicalListKey (map canonicalBackendPatternBinderKey binders) ++ ")"

canonicalBackendPatternBinderKey :: BackendPatternBinder -> String
canonicalBackendPatternBinderKey binder =
  canonicalTermRefKey (backendPatternBinderIdentity binder) (backendPatternBinderName binder)

canonicalListKey :: [String] -> String
canonicalListKey items =
  "[" ++ intercalate "," items ++ "]"

canonicalSymbolRefKey :: Maybe SymbolIdentity -> String -> String
canonicalSymbolRefKey (Just identity) _ =
  symbolIdentityStableName identity
canonicalSymbolRefKey Nothing name =
  "name:" ++ show name

canonicalClosureEntryKey :: Maybe UniqueIdentity -> String -> String
canonicalClosureEntryKey (Just identity) _ =
  "closure:" ++ show (uniqueIdentityValue identity)
canonicalClosureEntryKey Nothing name =
  "name:" ++ show name

canonicalTermRefKey :: Maybe IdDetails -> String -> String
canonicalTermRefKey Nothing name =
  "name:" ++ show name
canonicalTermRefKey (Just details) _ =
  case details of
    LocalId ref -> canonicalLocalRefKey ref
    EvidenceId ref -> canonicalLocalRefKey ref
    EnvId ref -> "env:" ++ show (envRefIdentity ref)
    TopLevelId symbol -> "top:" ++ symbolIdentityStableName symbol
    ConstructorId ref -> "ctor:" ++ symbolIdentityStableName (constructorRefSymbol ref)
    MethodId symbol -> "method:" ++ symbolIdentityStableName symbol
    PrimitiveId ref -> "primitive:" ++ symbolIdentityStableName (primitiveRefSymbol ref)
    DeferredId ref -> "deferred:" ++ show (deferredRefIdentity ref)

canonicalLocalRefKey :: LocalRef -> String
canonicalLocalRefKey ref =
  "local:" ++ show (localIdentityStableUnique (localRefIdentity ref))

collectFunctionWrappersInForm :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> Set TermBoundKey -> FunctionForm -> [(BackendType, BackendExpr)]
collectFunctionWrappersInForm base substitution bound form =
  collectFunctionWrappersInFormWithLocals base substitution emptyLocalStoredFunctions bound form

collectFunctionWrappersInFormWithLocals :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> LocalStoredFunctions -> Set TermBoundKey -> FunctionForm -> [(BackendType, BackendExpr)]
collectFunctionWrappersInFormWithLocals base substitution localFunctions bound form =
  let paramRefs = [(mbIdentity, name) | (mbIdentity, name, _) <- functionFormParamTriples form]
   in collectFunctionWrappersInExpr
        base
        substitution
        (shadowLocalStoredFunctions (map fst paramRefs) localFunctions)
        (Set.union (termBoundKeyRefs (map fst paramRefs)) bound)
        (ffBody form)

collectFunctionWrappersInExpr :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> LocalStoredFunctions -> Set TermBoundKey -> BackendExpr -> [(BackendType, BackendExpr)]
collectFunctionWrappersInExpr base substitution localFunctions bound expr =
  wrapperRequests ++ childRequests
  where
    wrapperRequests =
      case expr of
        BackendConstructWithIdentity resultTy mbIdentity name args ->
          case lookupConstructorRuntime base mbIdentity name >>= \constructorRuntime -> constructorRuntimeFieldTypes constructorRuntime resultTy of
            Just fieldTys ->
              [ request
              | (fieldTy, arg) <- zip fieldTys args,
                let fieldTy' = substituteBackendTypesByKey substitution fieldTy,
                let arg' = substituteExprTypesByKey substitution arg,
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
        (BackendVarWithIdentity _ mbIdentity _name, typeArgs) ->
          localStoredFunctionSourceByRef Set.empty mbIdentity typeArgs
        _ -> Nothing

    localStoredFunctionSourceByRef seen mbIdentity typeArgs
      | Just key <- mbKey,
        Set.member key seen =
          Nothing
      | Just key <- mbKey,
        Just localFunction <- lookupLocalStoredFunction mbIdentity localFunctions =
          case instantiateFunctionFormWithTypeArgs ("function wrapper request " ++ lsfName localFunction) (lsfForm localFunction) typeArgs [] of
            Right (_, instantiated) ->
              case etaAliasTarget instantiated of
                Just (targetIdentity, _targetName, targetTypeArgs) ->
                  localStoredFunctionSourceByRef (Set.insert key seen) targetIdentity targetTypeArgs
                Nothing ->
                  applyStoredSourceTypeArgs (lsfName localFunction) (lsfSourceExpr localFunction) typeArgs
            Left _ ->
              Nothing
      | otherwise =
          Nothing
      where
        mbKey = localStoredFunctionKey mbIdentity

    applyStoredSourceTypeArgs _ sourceExpr [] =
      Just sourceExpr
    applyStoredSourceTypeArgs name sourceExpr typeArgs =
      case applyTypeApplicationsToExprWithType ("function wrapper request " ++ name) sourceExpr typeArgs of
        Right (applied, _) -> Just applied
        Left _ -> Nothing

    childRequests =
      case expr of
        BackendVarWithIdentity {} ->
          []
        BackendLit {} ->
          []
        BackendLamWithIdentity _ mbIdentity _name _ body ->
          collectFunctionWrappersInExpr base substitution (deleteLocalStoredFunction mbIdentity localFunctions) (Set.union (termBoundKeys mbIdentity) bound) body
        BackendApp _ fun arg ->
          collectFunctionWrappersInExpr base substitution localFunctions bound fun
            ++ collectFunctionWrappersInExpr base substitution localFunctions bound arg
        BackendLetWithIdentity _ mbIdentity name _ rhs body ->
          collectFunctionWrappersInExpr base substitution localFunctions bound rhs
          ++ collectFunctionWrappersInExpr base substitution (collectLetLocalFunction mbIdentity name rhs) (Set.union (termBoundKeys mbIdentity) bound) body
        BackendTyAbs _ name _ body ->
          collectFunctionWrappersInExpr base (deleteTypeBinderSubstitution (backendTyAbsIdentity expr) name substitution) localFunctions bound body
        BackendTyApp _ fun _ ->
          collectFunctionWrappersInExpr base substitution localFunctions bound fun
        BackendConstructWithIdentity _ _ _ args ->
          concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound) args
        BackendCase _ scrutinee alternatives ->
          collectFunctionWrappersInExpr base substitution localFunctions bound scrutinee
            ++ concatMap collectAlternativeWrappers (NE.toList alternatives)
        BackendRoll _ payload ->
          collectFunctionWrappersInExpr base substitution localFunctions bound payload
        BackendUnroll _ payload ->
          collectFunctionWrappersInExpr base substitution localFunctions bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound . backendClosureCaptureExpr) captures
            ++ collectFunctionWrappersInExpr
              base
              substitution
              (shadowLocalStoredFunctions (map fst closureRefs) localFunctions)
              (Set.union (termBoundKeyRefs (map fst closureRefs)) bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
        BackendClosure _ _ captures params body ->
          concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound . backendClosureCaptureExpr) captures
            ++ collectFunctionWrappersInExpr
              base
              substitution
              (shadowLocalStoredFunctions (map fst closureRefs) localFunctions)
              (Set.union (termBoundKeyRefs (map fst closureRefs)) bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(Nothing, name) | (name, _) <- params]
        BackendClosureCall _ fun args ->
          collectFunctionWrappersInExpr base substitution localFunctions bound fun
            ++ concatMap (collectFunctionWrappersInExpr base substitution localFunctions bound) args

    collectLetLocalFunction mbIdentity name rhs =
      case functionFormFromExpected (backendExprType rhs) rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              bindLocalStoredFunction
                mbIdentity
                name
                (substituteFunctionFormTypes substitution form)
                (substituteExprTypesByKey substitution rhs)
                localFunctions
        _ ->
          deleteLocalStoredFunction mbIdentity localFunctions

    collectAlternativeWrappers alternative =
      let binderRefs = patternBinderRefs (backendAltPattern alternative)
       in collectFunctionWrappersInExpr
            base
            substitution
            (shadowLocalStoredFunctions (map fst binderRefs) localFunctions)
            (Set.union (termBoundKeyRefs (map fst binderRefs)) bound)
            (backendAltBody alternative)






isSimpleFunctionReference :: BackendExpr -> Bool
isSimpleFunctionReference arg =
  case collectTyApps arg of
    (BackendVarWithIdentity {}, _) -> True
    _ -> False

evidenceWrapperArgumentClosed :: Set TermBoundKey -> BackendExpr -> Bool
evidenceWrapperArgumentClosed bound expr =
  Set.null (freeTermVars expr `Set.intersection` bound)

freeTermVars :: BackendExpr -> Set TermBoundKey
freeTermVars =
  go Set.empty
  where
    go bound =
      \case
        BackendVarWithIdentity _ mbIdentity _name
          | Just _ <- backendVarSymbolIdentity mbIdentity -> Set.empty
          | Just key <- mbIdentity >>= lowerLocalKey -> localFreeTerm bound (TermBoundIdentity key)
          | otherwise -> Set.empty
        BackendLit {} ->
          Set.empty
        BackendLamWithIdentity _ mbIdentity _name _ body ->
          go (Set.union (termBoundKeys mbIdentity) bound) body
        BackendApp _ fun arg ->
          Set.union (go bound fun) (go bound arg)
        BackendLetWithIdentity _ mbIdentity _name _ rhs body ->
          Set.union (go bound rhs) (go (Set.union (termBoundKeys mbIdentity) bound) body)
        BackendTyAbs _ _ _ body ->
          go bound body
        BackendTyApp _ fun _ ->
          go bound fun
        BackendConstructWithIdentity _ _ _ args ->
          foldMap (go bound) args
        BackendCase _ scrutinee alternatives ->
          Set.union
            (go bound scrutinee)
            (foldMap (goAlternative bound) (NE.toList alternatives))
        BackendRoll _ payload ->
          go bound payload
        BackendUnroll _ payload ->
          go bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          foldMap (go bound . backendClosureCaptureExpr) captures
            `Set.union` go (Set.union closureBoundKeys bound) body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
            closureBoundKeys = termBoundKeyRefs (map fst closureRefs)
        BackendClosureCall _ fun args ->
          Set.union (go bound fun) (foldMap (go bound) args)

    goAlternative bound (BackendAlternative pattern0 body) =
      go (Set.union (patternTermBoundKeys pattern0) bound) body

    localFreeTerm bound key
      | Set.member key bound = Set.empty
      | otherwise = Set.singleton key



collectSpecializationRequestsInForm :: ProgramBase -> Map BackendTypeSubstitutionKey BackendType -> FunctionForm -> [SpecRequest]
collectSpecializationRequestsInForm base substitution form =
  collectSpecializationRequestsInFormWithBound base substitution Set.empty form

lookupSpecializationBinding :: ProgramBase -> Maybe IdDetails -> Maybe BindingInfo
lookupSpecializationBinding base mbIdentity =
  case (mbIdentity >>= lowerLocalKey, backendVarSymbolIdentity mbIdentity) of
    (Just _, _) ->
      Nothing
    (_, Just identity) ->
      Map.lookup identity (pbBindingsByIdentity base)
    _ ->
      Nothing

collectSpecializationRequestsInFormWithBound ::
  ProgramBase ->
  Map BackendTypeSubstitutionKey BackendType ->
  Set TermBoundKey ->
  FunctionForm ->
  [SpecRequest]
collectSpecializationRequestsInFormWithBound base substitution bound form =
  collectSpecializationRequestsWithBound
    base
    substitution
    (Set.union (termBoundKeyRefs [mbIdentity | (mbIdentity, _, _) <- functionFormParamTriples form]) bound)
    (ffBody form)

collectSpecializationRequestsWithBound ::
  ProgramBase ->
  Map BackendTypeSubstitutionKey BackendType ->
  Set TermBoundKey ->
  BackendExpr ->
  [SpecRequest]
collectSpecializationRequestsWithBound base substitution bound expr =
  requestHere ++ childRequests
  where
    requestHere =
      case expr of
        BackendApp {} ->
          case collectCall expr of
            Just (BackendVarWithIdentity _ mbIdentity name, typeArgs, args)
              | Just binding <- lookupSpecializationBinding base mbIdentity,
                not (null (ffTypeBinders (biForm binding))) ->
                  let typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs
                      args' = map (substituteExprTypesByKey substitution) args
                   in case instantiateFunctionFormWithTypeArgs ("specialization request " ++ name) (biForm binding) typeArgs' args' of
                        Right (resolvedTypeArgs, _) -> [specRequestForBinding binding resolvedTypeArgs]
                        Left _ -> []
            Just (fun, typeArgs, args) ->
              collectAdministrativeCallRequests fun typeArgs args
            _ -> []
        BackendTyApp {} ->
          case collectTyApps expr of
            (BackendVarWithIdentity _ mbIdentity name, typeArgs)
              | Just binding <- lookupSpecializationBinding base mbIdentity,
                not (null (ffTypeBinders (biForm binding))) ->
                  let typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs
                   in case instantiateFunctionFormWithTypeArgs ("specialization request " ++ name) (biForm binding) typeArgs' [] of
                        Right (resolvedTypeArgs, _) -> [specRequestForBinding binding resolvedTypeArgs]
                        _ -> []
            (fun, typeArgs) ->
              collectAdministrativeTypeAppRequests fun typeArgs
        _ -> []

    childRequests =
      case expr of
        BackendVarWithIdentity {} -> []
        BackendLit {} -> []
        BackendLamWithIdentity _ mbIdentity _name _ body ->
          collectSpecializationRequestsWithBound base substitution (Set.union (termBoundKeys mbIdentity) bound) body
        BackendApp _ fun arg ->
          collectSpecializationRequestsWithBound base substitution bound fun
            ++ collectSpecializationRequestsWithBound base substitution bound arg
        BackendLetWithIdentity _ mbIdentity _name bindingTy rhs body ->
          collectLetRhsSpecializationRequests bindingTy rhs
            ++ collectSpecializationRequestsWithBound base substitution (Set.union (termBoundKeys mbIdentity) bound) body
        BackendTyAbs _ name _ body ->
          collectSpecializationRequestsWithBound base (deleteTypeBinderSubstitution (backendTyAbsIdentity expr) name substitution) bound body
        BackendTyApp {} ->
          []
        BackendConstructWithIdentity _ _ _ args ->
          concatMap (collectSpecializationRequestsWithBound base substitution bound) args
        BackendCase _ scrutinee alternatives ->
          collectSpecializationRequestsWithBound base substitution bound scrutinee
            ++ concatMap collectAlternativeRequests (NE.toList alternatives)
        BackendRoll _ payload ->
          collectSpecializationRequestsWithBound base substitution bound payload
        BackendUnroll _ payload ->
          collectSpecializationRequestsWithBound base substitution bound payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          concatMap (collectSpecializationRequestsWithBound base substitution bound . backendClosureCaptureExpr) captures
            ++ collectSpecializationRequestsWithBound
              base
              substitution
              (Set.union (termBoundKeyRefs (map fst closureRefs)) bound)
              body
          where
            closureRefs =
              [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
        BackendClosureCall _ fun args ->
          collectSpecializationRequestsWithBound base substitution bound fun
            ++ concatMap (collectSpecializationRequestsWithBound base substitution bound) args

    collectAlternativeRequests alternative =
      collectSpecializationRequestsWithBound
        base
        substitution
        (Set.union (patternTermBoundKeys (backendAltPattern alternative)) bound)
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
                (Set.union (termBoundKeyRefs [mbIdentity | (mbIdentity, _, _) <- functionFormParamTriples form]) bound)
                (ffBody form)
            Left _ ->
              []
        Left _ ->
          []
      where
        context = "specialization request"
        resultTy = substituteBackendTypesByKey substitution (backendExprType expr)
        fun' = substituteExprTypesByKey substitution fun
        typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs

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
        resultTy = substituteBackendTypesByKey substitution (backendExprType expr)
        fun' = substituteExprTypesByKey substitution fun
        typeArgs' = map (substituteBackendTypesByKey substitution) typeArgs
        args' = map (substituteExprTypesByKey substitution) args



freeGlobalBindingRefs :: ProgramBase -> BindingInfo -> Set BackendBindingRef
freeGlobalBindingRefs base binding =
  freeGlobalRefs
    base
    (termBoundKeyRefs [mbIdentity | (mbIdentity, _, _) <- functionFormParamTriples (biForm binding)])
    (ffBody (biForm binding))

freeGlobalRefs :: ProgramBase -> Set TermBoundKey -> BackendExpr -> Set BackendBindingRef
freeGlobalRefs base bound expr =
  case expr of
    BackendVarWithIdentity _ mbIdentity _name
      | Just binding <- lookupSpecializationBinding base mbIdentity -> Set.singleton (bindingInfoRef binding)
      | otherwise -> Set.empty
    BackendLit {} ->
      Set.empty
    BackendLamWithIdentity _ mbIdentity _name _ body ->
      freeGlobalRefs base (Set.union (termBoundKeys mbIdentity) bound) body
    BackendApp _ fun arg ->
      freeGlobalRefs base bound fun `Set.union` freeGlobalRefs base bound arg
    BackendLetWithIdentity _ mbIdentity _name _ rhs body ->
      freeGlobalRefs base bound rhs `Set.union` freeGlobalRefs base (Set.union (termBoundKeys mbIdentity) bound) body
    BackendTyAbs _ _ _ body ->
      freeGlobalRefs base bound body
    BackendTyApp _ fun _ ->
      freeGlobalRefs base bound fun
    BackendConstructWithIdentity _ _ _ args ->
      Set.unions (map (freeGlobalRefs base bound) args)
    BackendCase _ scrutinee alternatives ->
      freeGlobalRefs base bound scrutinee
        `Set.union` Set.unions (map (freeAlternativeRefs bound) (NE.toList alternatives))
    BackendRoll _ payload ->
      freeGlobalRefs base bound payload
    BackendUnroll _ payload ->
      freeGlobalRefs base bound payload
    BackendClosureWithParamIdentities _ _ _ captures params body ->
      Set.unions (map (freeGlobalRefs base bound . backendClosureCaptureExpr) captures)
        `Set.union` freeGlobalRefs base (Set.union (termBoundKeyRefs (map fst closureRefs)) bound) body
      where
        closureRefs =
          [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
            ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
    BackendClosureCall _ fun args ->
      freeGlobalRefs base bound fun `Set.union` Set.unions (map (freeGlobalRefs base bound) args)
  where
    freeAlternativeRefs bound0 alternative =
      freeGlobalRefs base (Set.union (patternTermBoundKeys (backendAltPattern alternative)) bound0) (backendAltBody alternative)



collectProgramStrings :: [BindingInfo] -> [Specialization] -> [Wrapper] -> [Wrapper] -> [String]
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
    BackendVarWithIdentity {} -> []
    BackendLit _ (LString value) -> [value]
    BackendLit {} -> []
    BackendLam _ _ _ body -> collectStringLiterals body
    BackendApp _ fun arg -> collectStringLiterals fun ++ collectStringLiterals arg
    BackendLet _ _ _ rhs body -> collectStringLiterals rhs ++ collectStringLiterals body
    BackendTyAbs _ _ _ body -> collectStringLiterals body
    BackendTyApp _ fun _ -> collectStringLiterals fun
    BackendConstructWithIdentity _ _ _ args -> concatMap collectStringLiterals args
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

nativeStringLiteralSupported :: String -> Bool
nativeStringLiteralSupported =
  all (\char -> ord char <= 0x7FF)

nativeStringByteLength :: String -> Int
nativeStringByteLength =
  sum . map nativeUtf8ByteLength

nativeUtf8ByteLength :: Char -> Int
nativeUtf8ByteLength char
  | code <= 0x7F = 1
  | code <= 0x7FF = 2
  | code <= 0xFFFF = 3
  | otherwise = 4
  where
    code = ord char

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
  specRequestBindingKey request ++ "\0" ++ intercalate "\0" (map backendTypeKey (srTypeArgs request))

specRequestBindingKey :: SpecRequest -> String
specRequestBindingKey request =
  maybe (srBindingName request) symbolIdentityStableName (srBindingIdentity request)

specializedFunctionName :: SpecRequest -> String
specializedFunctionName request =
  srBindingName request ++ "$" ++ intercalate "$" (map backendTypeKey (srTypeArgs request))

backendTypeKey :: BackendType -> String
backendTypeKey =
  ("t" ++) . intercalate "_" . map (flip showHex "" . ord) . canonicalBackendTypeKey

canonicalBackendTypeKey :: BackendType -> String
canonicalBackendTypeKey =
  \case
    BTVarWithIdentity identity name ->
      "var(" ++ canonicalTypeBinderKey identity name ++ ")"
    BTArrow dom cod ->
      "arrow(" ++ canonicalBackendTypeKey dom ++ "," ++ canonicalBackendTypeKey cod ++ ")"
    BTBaseWithIdentity identity base ->
      "base(" ++ canonicalTypeHeadKey identity base ++ ")"
    BTConWithIdentity identity base args ->
      "con(" ++ canonicalTypeHeadKey identity base ++ "," ++ intercalate "," (map canonicalBackendTypeKey (NE.toList args)) ++ ")"
    BTVarAppWithIdentity identity name args ->
      "varapp(" ++ canonicalTypeBinderKey identity name ++ "," ++ intercalate "," (map canonicalBackendTypeKey (NE.toList args)) ++ ")"
    BTForallWithIdentity identity name mbBound body ->
      "forall(" ++ canonicalTypeBinderKey identity name ++ "," ++ maybe "_" canonicalBackendTypeKey mbBound ++ "," ++ canonicalBackendTypeKey body ++ ")"
    BTMuWithIdentity identity name body ->
      "mu(" ++ canonicalTypeBinderKey identity name ++ "," ++ canonicalBackendTypeKey body ++ ")"
    BTBottom ->
      "bottom"

canonicalTypeBinderKey :: Maybe TypeBinderIdentity -> String -> String
canonicalTypeBinderKey (Just identity) _ =
  typeBinderIdentityStableName identity
canonicalTypeBinderKey Nothing name =
  "name:" ++ name

canonicalTypeHeadKey :: Maybe SymbolIdentity -> BaseTy -> String
canonicalTypeHeadKey (Just identity) _ =
  symbolIdentityStableName identity
canonicalTypeHeadKey Nothing (BaseTy name) =
  "name:" ++ name

lowerValueKindKey :: LowerValueKind -> String
lowerValueKindKey =
  \case
    LowerRuntimeValue -> "runtime"
    LowerClosureRecord -> "closure"
    LowerFunctionPointer -> "function"

returnedPartialClosureEntryName :: LowerValueKind -> BackendType -> Int -> [LowerValueKind] -> BackendType -> String
returnedPartialClosureEntryName calleeKind calleeTy suppliedCount suppliedKinds resultTy =
  intercalate
    "$"
    [ "__mlfp_returned_partial",
      lowerValueKindKey calleeKind,
      show suppliedCount,
      intercalate "_" (map lowerValueKindKey suppliedKinds),
      backendTypeKey calleeTy,
      backendTypeKey resultTy
    ]

returnedPartialCalleeCaptureName :: String
returnedPartialCalleeCaptureName =
  "__mlfp_returned_partial_callee"

returnedPartialSuppliedArgName :: Int -> String
returnedPartialSuppliedArgName index0 =
  "__mlfp_returned_partial_supplied" ++ show index0

returnedPartialParamName :: Int -> String
returnedPartialParamName index0 =
  "__mlfp_returned_partial_param" ++ show index0

functionTypeFromParts :: [BackendType] -> BackendType -> BackendType
functionTypeFromParts params returnTy =
  foldr BTArrow returnTy params

lowerFunctionJobs :: IdentityGenerator -> [IdentityGenerator -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)] -> Either BackendLLVMError ([LLVMFunction], IdentityGenerator)
lowerFunctionJobs generator0 jobs =
  foldM runJob ([], generator0) jobs >>= \(functionsRev, generator') ->
    Right (reverse functionsRev, generator')
  where
    runJob (functionsRev, generator) job = do
      (generator', function) <- job generator
      Right (function : functionsRev, generator')

lowerMonomorphicBinding :: ProgramEnv -> BindingInfo -> IdentityGenerator -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)
lowerMonomorphicBinding env binding generator =
  lowerFunction env generator (bindingInfoRef binding) (biName binding) False (biForm binding)

lowerSpecialization :: ProgramEnv -> Specialization -> IdentityGenerator -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)
lowerSpecialization env specialization generator =
  lowerFunction env generator (spBindingRef specialization) (spFunctionName specialization) True (qualifiedSpecializationForm specialization)

lowerEvidenceWrapper :: ProgramEnv -> Wrapper -> IdentityGenerator -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)
lowerEvidenceWrapper env wrapper generator =
  lowerFunction env generator (wrapperBindingRef wrapper) (wrapperFunctionName wrapper) True (qualifiedEvidenceWrapperForm wrapper)

lowerFunctionWrapper :: ProgramEnv -> Wrapper -> IdentityGenerator -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)
lowerFunctionWrapper env wrapper generator =
  lowerFunction env generator (wrapperBindingRef wrapper) (wrapperFunctionName wrapper) True (qualifiedFunctionWrapperForm wrapper)

collectClosureEntries :: ProgramBase -> [BindingInfo] -> [Specialization] -> [Wrapper] -> [Wrapper] -> [ClosureEntry]
collectClosureEntries base reachable specializations evidenceWrappers functionWrappers =
  concatMap (collectClosureEntriesInForm base . biForm) (filter (null . ffTypeBinders . biForm) reachable)
    ++ concatMap (collectClosureEntriesInForm base . qualifiedSpecializationForm) specializations
    ++ concatMap (collectClosureEntriesInForm base . qualifiedEvidenceWrapperForm) evidenceWrappers
    ++ concatMap (collectClosureEntriesInForm base . qualifiedFunctionWrapperForm) functionWrappers

requireUniqueClosureEntries :: [ClosureEntry] -> Either BackendLLVMError [ClosureEntry]
requireUniqueClosureEntries entries =
  reverse <$> foldM includeEntry [] entries
  where
    includeEntry kept entry =
      case find (`sameClosureEntryRef` entry) kept of
        Just existing
          | existing == entry ->
              Right kept
          | otherwise ->
              duplicateEntry
        Nothing ->
          case find ((== ceEntryName entry) . ceEntryName) kept of
            Just {} -> duplicateEntry
            Nothing -> Right (entry : kept)
      where
        duplicateEntry =
          Left (BackendLLVMInternalError ("duplicate closure entry after specialization: " ++ ceEntryName entry))

    sameClosureEntryRef left right =
      closureEntryRefMatches (ceEntryIdentity left) (ceEntryName left) (ceEntryIdentity right) (ceEntryName right)

assignGeneratedClosureEntryIdentities :: IdentityGenerator -> [ClosureEntry] -> (IdentityGenerator, [ClosureEntry])
assignGeneratedClosureEntryIdentities =
  mapAccumL assignEntry
  where
    assignEntry generator entry
      | returnedPartialClosureEntry entry || ceEntryIdentity entry == Nothing =
          let captureInputs = [(ccsName capture, ccsIdentity capture) | capture <- ceCaptures entry]
              paramInputs = zip (map fst (ceParams entry)) (ceParamIdentities entry ++ repeat Nothing)
              (generator', captureIdentities, generatedCaptures) = completeLocalIdentities generator captureInputs
              (generator'', paramIdentities, generatedParams) = completeLocalIdentities generator' paramInputs
              (entryIdentity, generator''') =
                case ceEntryIdentity entry of
                  Just identity -> (identity, generator'')
                  Nothing -> freshIdentity generator''
              identities =
                generatedBackendTermEnv (generatedCaptures ++ generatedParams)
           in ( generator''',
                entry
                  { ceEntryIdentity = Just entryIdentity,
                    ceCaptures = zipWith setCaptureIdentity captureIdentities (ceCaptures entry),
                    ceParamIdentities = paramIdentities,
                    ceBody = rewriteBackendVarsByName identities (ceBody entry)
                  }
              )
      | otherwise =
          (generator, entry)

    returnedPartialClosureEntry entry =
      "__mlfp_returned_partial$" `isPrefixOf` ceEntryName entry

    setCaptureIdentity identity capture =
      capture {ccsIdentity = identity}

    completeLocalIdentities generator [] =
      (generator, [], [])
    completeLocalIdentities generator ((name, mbIdentity) : rest) =
      let (identity, generator') =
            case mbIdentity of
              Just existing -> (existing, generator)
              Nothing ->
                let (localRef, nextGenerator) = freshLocalRef name generator
                 in (LocalId localRef, nextGenerator)
          (generator'', identities, generated) = completeLocalIdentities generator' rest
          generated' =
            case mbIdentity of
              Just _ -> generated
              Nothing -> (name, Just identity) : generated
       in (generator'', Just identity : identities, generated')

qualifiedSpecializationForm :: Specialization -> FunctionForm
qualifiedSpecializationForm specialization =
  qualifyClosureEntriesInForm (spFunctionName specialization) (spForm specialization)

qualifiedEvidenceWrapperForm :: Wrapper -> FunctionForm
qualifiedEvidenceWrapperForm wrapper =
  qualifyClosureEntriesInForm (wrapperFunctionName wrapper) (evidenceWrapperForm wrapper)

qualifiedFunctionWrapperForm :: Wrapper -> FunctionForm
qualifiedFunctionWrapperForm wrapper =
  qualifyClosureEntriesInForm (wrapperFunctionName wrapper) (functionWrapperForm wrapper)

qualifyClosureEntriesInForm :: String -> FunctionForm -> FunctionForm
qualifyClosureEntriesInForm ownerName form =
  form {ffBody = qualifyClosureEntriesInExpr ownerName (ffBody form)}

qualifyInstantiatedClosureEntries :: String -> [BackendType] -> FunctionForm -> FunctionForm
qualifyInstantiatedClosureEntries ownerName resolvedTypeArgs form
  | null resolvedTypeArgs = form
  | otherwise = qualifyClosureEntriesInForm (closureEntryOwnerName ownerName resolvedTypeArgs) form

qualifyInstantiatedClosureEntriesWithParamKinds :: String -> [BackendType] -> LocalValueKinds -> FunctionForm -> FunctionForm
qualifyInstantiatedClosureEntriesWithParamKinds ownerName resolvedTypeArgs suppliedParamKinds form
  | null resolvedTypeArgs && null firstOrderParamKinds = form
  | otherwise = qualifyClosureEntriesInForm (closureEntryOwnerNameWithParamKinds ownerName resolvedTypeArgs firstOrderParamKinds) form
  where
    firstOrderParamKinds =
      [ suppliedKind
      | (mbIdentity, _paramName, paramTy) <- functionFormParamTriples form,
        isFirstOrderFunctionPointerType paramTy,
        Just suppliedKind <- [lookupLocalValueKind mbIdentity suppliedParamKinds]
      ]

qualifyClosureEntriesInExpr :: String -> BackendExpr -> BackendExpr
qualifyClosureEntriesInExpr ownerName =
  go
  where
    go =
      \case
        BackendVarWithIdentity resultTy mbIdentity name ->
          BackendVarWithIdentity resultTy mbIdentity name
        BackendLit resultTy lit ->
          BackendLit resultTy lit
        BackendLamWithIdentity resultTy mbIdentity name paramTy body ->
          BackendLamWithIdentity resultTy mbIdentity name paramTy (go body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go fun) (go arg)
        BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs body ->
          BackendLetWithIdentity resultTy mbIdentity name bindingTy (go rhs) (go body)
        BackendTyAbsWithIdentity resultTy identity name mbBound body ->
          BackendTyAbsWithIdentity resultTy identity name mbBound (go body)
        BackendTyApp resultTy fun ty ->
          BackendTyApp resultTy (go fun) ty
        BackendConstructWithIdentity resultTy mbIdentity name args ->
          BackendConstructWithIdentity resultTy mbIdentity name (map go args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go scrutinee) (fmap qualifyAlternative alternatives)
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go payload)
        BackendClosureWithParamIdentities resultTy _entryIdentity entryName captures params body ->
          BackendClosureWithParamIdentities
            resultTy
            Nothing
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

closureEntryOwnerNameWithParamKinds :: String -> [BackendType] -> [LowerValueKind] -> String
closureEntryOwnerNameWithParamKinds name typeArgs firstOrderParamKinds =
  closureEntryOwnerName name typeArgs
    ++ if null firstOrderParamKinds
      then ""
      else "$vk$" ++ intercalate "_" (map lowerValueKindKey firstOrderParamKinds)

collectClosureEntriesInForm :: ProgramBase -> FunctionForm -> [ClosureEntry]
collectClosureEntriesInForm base =
  collectClosureEntriesInFormWithLocals base emptyLocalFunctionForms

collectClosureEntriesInFormWithLocals :: ProgramBase -> LocalFunctionForms -> FunctionForm -> [ClosureEntry]
collectClosureEntriesInFormWithLocals base localForms form =
  collectClosureEntriesInFormWithParamKinds base localForms emptyLocalValueKinds form

collectClosureEntriesInFormWithParamKinds :: ProgramBase -> LocalFunctionForms -> LocalValueKinds -> FunctionForm -> [ClosureEntry]
collectClosureEntriesInFormWithParamKinds base localForms suppliedParamKinds form =
  collectClosureEntriesInExpr base (shadowLocalFunctionForms (map fst paramRefs) localForms) paramValueKinds (ffBody form)
  where
    paramRefs = [(mbIdentity, name) | (mbIdentity, name, _) <- functionFormParamTriples form]
    paramValueKinds =
      foldr bindParam emptyLocalValueKinds (indexed (functionFormParamTriples form))

    bindParam (index0, (mbIdentity, _paramName, paramTy)) =
      bindLocalValueKind
        mbIdentity
        (fromMaybe (parameterValueKind (ffEvidenceParams form) index0 paramTy) (lookupLocalValueKind mbIdentity suppliedParamKinds))

collectClosureEntriesInExpr :: ProgramBase -> LocalFunctionForms -> LocalValueKinds -> BackendExpr -> [ClosureEntry]
collectClosureEntriesInExpr base localForms valueKinds expr =
  case expr of
    BackendVarWithIdentity {} -> []
    BackendLit {} -> []
    BackendLamWithIdentity _ mbIdentity _name paramTy body ->
      collectClosureEntriesInExpr
        base
        (deleteLocalFunctionForm mbIdentity localForms)
        (bindLocalValueKind mbIdentity (localFunctionParameterValueKind Set.empty 0 paramTy) valueKinds)
        body
    BackendApp _ fun arg ->
      case collectAdministrativeCallEntries of
        Just entries -> entries
        Nothing ->
          collectAppliedLocalClosureEntries
            ++ collectReturnedPartialClosureEntries
            ++ collectClosureEntriesInExpr base localForms valueKinds fun
            ++ collectClosureEntriesInExpr base localForms valueKinds arg
    BackendLetWithIdentity _ mbIdentity name bindingTy rhs body ->
      rhsEntries ++ collectClosureEntriesInExpr base bodyLocalForms bodyValueKinds body
      where
        rhsEntries =
          case functionFormFromExpected bindingTy rhs of
            form
              | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
                  -- Function body entries can depend on whether arguments arrive
                  -- as raw function pointers or closure records, so collect them
                  -- from call sites where supplied argument kinds are known.
                  []
            _ ->
              collectClosureEntriesInExpr base localForms valueKinds rhs
        bodyLocalForms = collectLetLocalForm localForms mbIdentity name bindingTy rhs
        bodyValueKinds =
          case letBoundValueKind Set.empty localForms valueKinds bindingTy rhs of
            Just kind -> bindLocalValueKind mbIdentity kind valueKinds
            Nothing -> deleteLocalValueKind mbIdentity valueKinds
    BackendTyAbs _ _ _ _ -> []
    BackendTyApp {} ->
      case collectAdministrativeTypeAppEntries of
        Just entries -> entries
        Nothing -> collectTypeAppliedClosureEntries
    BackendConstructWithIdentity resultTy mbIdentity name args ->
      concatMap (collectClosureEntriesInExpr base localForms valueKinds) args
        ++ collectConstructorFieldAdapterEntries resultTy mbIdentity name args
    BackendCase resultTy scrutinee alternatives ->
      collectClosureEntriesInExpr base localForms valueKinds scrutinee
        ++ concatMap collectAlternativeEntries (NE.toList alternatives)
        ++ collectCaseResultAdapterEntries resultTy (NE.toList alternatives)
    BackendRoll _ payload -> collectClosureEntriesInExpr base localForms valueKinds payload
    BackendUnroll _ payload -> collectClosureEntriesInExpr base localForms valueKinds payload
    BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
      closureEntriesFor resultTy entryIdentity entryName captures params body
    BackendClosure resultTy entryName captures params body ->
      closureEntriesFor resultTy Nothing entryName captures (backendClosureParams params) body
    BackendClosureCall _ fun args ->
      collectClosureEntriesInExpr base localForms valueKinds fun ++ concatMap (collectClosureEntriesInExpr base localForms valueKinds) args
  where
    closureEntriesFor resultTy entryIdentity entryName captures params body =
      ClosureEntry
        { ceFunctionType = resultTy,
          ceEntryIdentity = entryIdentity,
          ceEntryName = entryName,
          ceCaptures = captureSlots,
          ceParams = [(backendClosureParamName param, backendClosureParamType param) | param <- params],
          ceParamIdentities = map backendClosureParamIdentity params,
          ceEvidenceParams = Set.empty,
          ceBody = body
        }
        : concatMap (collectClosureEntriesInExpr base localForms valueKinds . backendClosureCaptureExpr) captures
          ++ collectClosureEntriesInExpr base (shadowLocalFunctionForms (map fst closureRefs) localForms) closureBodyValueKinds body
      where
        captureSlots = map (closureCaptureSlot localForms valueKinds) captures
        closureRefs =
          [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
            ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
        closureBodyValueKinds =
          closureLocalValueKinds `unionLocalValueKinds` shadowLocalValueKinds (map fst closureRefs) valueKinds
        closureLocalValueKinds =
          foldr bindClosureParam closureCaptureValueKinds (indexed params)
        closureCaptureValueKinds =
          foldr bindClosureCapture emptyLocalValueKinds captureSlots
        bindClosureCapture capture =
          bindLocalValueKind (ccsIdentity capture) (ccsValueKind capture)
        bindClosureParam (index0, param) =
          bindLocalValueKind
            (backendClosureParamIdentity param)
            (parameterValueKind Set.empty index0 (backendClosureParamType param))

    collectAppliedLocalClosureEntries =
      case collectCall expr of
        Just (BackendVarWithIdentity _ mbIdentity _name, typeArgs, args)
          | Just entry <- lookupLocalFunctionFormEntry mbIdentity localForms ->
              collectInstantiatedLocalClosureEntries (lffeName entry) (lffeForm entry) typeArgs args
        _ -> []

    collectReturnedPartialClosureEntries =
      case collectCall expr of
        Just (headExpr, typeArgs, args)
          | Just form <- returnedPartialHeadForm headExpr ->
              case instantiateFunctionFormWithTypeArgs "returned partial closure entry collection" form typeArgs args of
                Right (_, instantiated) ->
                  returnedPartialEntriesForExtraArgs (ffReturnType instantiated) (drop (length (ffParams instantiated)) args) (backendExprType expr)
                Left _ -> []
        _ -> []

    returnedPartialHeadForm =
      \case
        BackendVarWithIdentity _ mbIdentity _name
          | Just form <- lookupLocalFunctionForm mbIdentity localForms ->
              Just form
          | Just binding <- lookupNonLocalBindingInfo base mbIdentity ->
              Just (biForm binding)
        headExpr@(BackendLam _ _ _ _) ->
          Just (functionFormFromExpr headExpr)
        headExpr@(BackendTyAbs _ _ _ _) ->
          Just (functionFormFromExpr headExpr)
        _ ->
          Nothing

    returnedPartialEntriesForExtraArgs calleeTy args resultTy =
      let (paramTys, returnTy) = collectArrowsType calleeTy
       in case compare (length args) (length paramTys) of
            LT
              | not (null args),
                not (null paramTys) ->
                  returnedPartialEntries calleeTy args resultTy
            GT
              | isFunctionLikeBackendType returnTy ->
                  returnedPartialEntriesForExtraArgs returnTy (drop (length paramTys) args) resultTy
            _ ->
              []

    returnedPartialEntries calleeTy args resultTy =
      [ returnedPartialEntry calleeKind calleeTy suppliedParamTys remainingParamTys finalReturnTy suppliedKinds resultTy
      | not (null args),
        length args < length paramTys,
        alphaEqBackendType resultTy expectedResultTy,
        calleeKind <- LowerClosureRecord : [LowerFunctionPointer | isFirstOrderFunctionPointerType calleeTy]
      ]
      where
        (paramTys, finalReturnTy) = collectArrowsType calleeTy
        suppliedCount = length args
        suppliedParamTys = take suppliedCount paramTys
        remainingParamTys = drop suppliedCount paramTys
        expectedResultTy = functionTypeFromParts remainingParamTys finalReturnTy
        suppliedKinds =
          [ argumentValueKind localForms valueKinds Set.empty index0 paramTy arg
          | (index0, (paramTy, arg)) <- zip [0 :: Int ..] (zip suppliedParamTys args)
          ]

    returnedPartialEntry calleeKind calleeTy suppliedParamTys remainingParamTys finalReturnTy suppliedKinds resultTy =
      ClosureEntry
        { ceFunctionType = resultTy,
          ceEntryIdentity = Nothing,
          ceEntryName = returnedPartialClosureEntryName calleeKind calleeTy (length suppliedParamTys) suppliedKinds resultTy,
          ceCaptures =
            ClosureCaptureSlot Nothing returnedPartialCalleeCaptureName calleeTy calleeKind
              : [ ClosureCaptureSlot Nothing (returnedPartialSuppliedArgName index0) paramTy suppliedKind
                  | (index0, (paramTy, suppliedKind)) <- zip [0 :: Int ..] (zip suppliedParamTys suppliedKinds)
                ],
          ceParams = remainingParams,
          ceParamIdentities = replicate (length remainingParams) Nothing,
          ceEvidenceParams = Set.empty,
          ceBody = returnedPartialBody calleeKind calleeTy suppliedParamTys remainingParams finalReturnTy
        }
      where
        remainingParams =
          [(returnedPartialParamName index0, paramTy) | (index0, paramTy) <- zip [0 :: Int ..] remainingParamTys]

    returnedPartialBody calleeKind calleeTy suppliedParamTys remainingParams finalReturnTy =
      case calleeKind of
        LowerClosureRecord ->
          BackendClosureCall finalReturnTy calleeExpr callArgs
        LowerFunctionPointer ->
          applyReturnedPartialArgs calleeExpr calleeTy callArgs
        LowerRuntimeValue ->
          BackendVar finalReturnTy "__mlfp_unreachable_returned_partial"
      where
        calleeExpr = BackendVar calleeTy returnedPartialCalleeCaptureName
        suppliedArgs =
          [ BackendVar paramTy (returnedPartialSuppliedArgName index0)
          | (index0, paramTy) <- zip [0 :: Int ..] suppliedParamTys
          ]
        remainingArgs =
          [BackendVar paramTy paramName | (paramName, paramTy) <- remainingParams]
        callArgs = suppliedArgs ++ remainingArgs

    applyReturnedPartialArgs fun _ [] =
      fun
    applyReturnedPartialArgs fun funTy (arg : rest) =
      case funTy of
        BTArrow _ resultTy ->
          applyReturnedPartialArgs (BackendApp resultTy fun arg) resultTy rest
        _ ->
          fun

    collectAdministrativeCallEntries =
      case collectCall expr of
        Just (headExpr, typeArgs, args) ->
          case pushCallIntoExpression "closure entry collection" (backendExprType expr) headExpr typeArgs args of
            Right (Just applied) -> Just (collectClosureEntriesInExpr base localForms valueKinds applied)
            _ -> Nothing
        Nothing -> Nothing

    collectTypeAppliedClosureEntries =
      case collectTyApps expr of
        (BackendVarWithIdentity _ mbIdentity _name, typeArgs)
          | Just entry <- lookupLocalFunctionFormEntry mbIdentity localForms ->
              collectInstantiatedLocalClosureEntries (lffeName entry) (lffeForm entry) typeArgs []
        (fun@(BackendTyAbs _ _ _ _), typeArgs) ->
          collectInstantiatedClosureEntries
            "__mlfp_direct_typeapp"
            (functionFormFromExpr fun)
            typeArgs
            []
        (fun, _) ->
          collectClosureEntriesInExpr base localForms valueKinds fun

    collectAdministrativeTypeAppEntries =
      case collectTyApps expr of
        (headExpr, typeArgs) ->
          case pushTypeApplicationsIntoExpression "closure entry collection" (backendExprType expr) headExpr typeArgs of
            Right (Just applied) -> Just (collectClosureEntriesInExpr base localForms valueKinds applied)
            _ -> Nothing

    collectInstantiatedLocalClosureEntries name form typeArgs args =
      collectInstantiatedClosureEntries name form typeArgs args

    collectInstantiatedClosureEntries ownerName form typeArgs args =
      case instantiateFunctionFormWithTypeArgs ("closure entry collection " ++ ownerName) form typeArgs args of
        Right (resolvedTypeArgs, instantiated) ->
          collectClosureEntriesInFormWithParamKinds
            base
            localForms
            suppliedParamKinds
            (qualifyInstantiatedClosureEntriesWithParamKinds ownerName resolvedTypeArgs suppliedParamKinds instantiated)
          where
            suppliedParamKinds = suppliedArgumentValueKinds instantiated args
        Left _ ->
          []

    collectLetLocalForm localForms0 mbIdentity name bindingTy rhs =
      case functionFormFromExpected bindingTy rhs of
        form
          | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
              bindLocalFunctionForm mbIdentity name form localForms0
        _ ->
          deleteLocalFunctionForm mbIdentity localForms0

    letBoundValueKind visitedGlobals localForms0 valueKinds0 bindingTy rhs =
      case aliasValueKind visitedGlobals localForms0 valueKinds0 rhs of
        Just kind ->
          Just kind
        Nothing ->
          case functionFormFromExpected bindingTy rhs of
            form
              | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
                  Nothing
            _ ->
              Just (expressionValueKindWith visitedGlobals localForms0 valueKinds0 rhs)

    closureCaptureSlot localForms0 valueKinds0 capture =
      ClosureCaptureSlot
        { ccsIdentity = backendClosureCaptureIdentity capture,
          ccsName = backendClosureCaptureName capture,
          ccsType = captureTy,
          ccsValueKind = captureValueKind
        }
      where
        captureTy = backendClosureCaptureType capture
        captureValueKind
          | captureKnownFunctionPointer valueKinds0 capture =
              LowerFunctionPointer
          | not (isClosureRuntimeValueType captureTy) =
              LowerRuntimeValue
          | shouldStoreFunctionPointerCapture localForms0 valueKinds0 capture =
              LowerFunctionPointer
          | otherwise =
              LowerClosureRecord

    shouldStoreFunctionPointerCapture localForms0 valueKinds0 capture =
      isFirstOrderFunctionPointerType (backendClosureCaptureType capture)
        && not (captureKnownFunctionPointer valueKinds0 capture)
        && aliasValueKind Set.empty localForms0 valueKinds0 (backendClosureCaptureExpr capture) /= Just LowerClosureRecord
        && case collectTyApps (backendClosureCaptureExpr capture) of
          (BackendVarWithIdentity {}, _) -> True
          _ -> False

    captureKnownFunctionPointer valueKinds0 capture =
      lookupLocalValueKind (backendClosureCaptureIdentity capture) valueKinds0 == Just LowerFunctionPointer
        && isFunctionLikeBackendType (backendClosureCaptureType capture)

    suppliedArgumentValueKinds instantiated args =
      foldr bindArg emptyLocalValueKinds (indexed (zip (functionFormParamTriples instantiated) args))
      where
        bindArg (index0, ((mbIdentity, _paramName, paramTy), arg)) =
          bindLocalValueKind
            mbIdentity
            (argumentValueKind localForms valueKinds (ffEvidenceParams instantiated) index0 paramTy arg)

    argumentValueKind localForms0 valueKinds0 evidenceParams index0 paramTy arg
      | isEvidenceArgument evidenceParams index0 paramTy =
          LowerFunctionPointer
      | isFunctionLikeBackendType paramTy =
          case arg of
            BackendClosure _ _ _ _ _ -> LowerClosureRecord
            _ ->
              case aliasValueKind Set.empty localForms0 valueKinds0 arg of
                Just kind -> kind
                Nothing ->
                  case collectTyApps arg of
                    (BackendVarWithIdentity {}, _) -> valueKindForType paramTy
                    _ -> LowerClosureRecord
      | otherwise =
          LowerRuntimeValue

    aliasValueKind visitedGlobals localForms0 valueKinds0 expr0 =
      case expr0 of
        BackendVarWithIdentity ty mbIdentity name
          | isFunctionLikeBackendType ty ->
              variableValueKind visitedGlobals localForms0 valueKinds0 mbIdentity name []
        BackendTyApp ty fun _
          | isFunctionLikeBackendType ty ->
              case collectTyApps expr0 of
                (BackendVarWithIdentity _ mbIdentity name, typeArgs) ->
                  variableValueKind visitedGlobals localForms0 valueKinds0 mbIdentity name typeArgs
                _ ->
                  aliasValueKind visitedGlobals localForms0 valueKinds0 fun
        BackendLetWithIdentity ty mbIdentity name bindingTy rhs body
          | isFunctionLikeBackendType ty ->
              let localForms' = collectLetLocalForm localForms0 mbIdentity name bindingTy rhs
                  valueKinds' =
                    case letBoundValueKind visitedGlobals localForms0 valueKinds0 bindingTy rhs of
                      Just kind -> bindLocalValueKind mbIdentity kind valueKinds0
                      Nothing -> deleteLocalValueKind mbIdentity valueKinds0
               in aliasValueKind visitedGlobals localForms' valueKinds' body
        _ ->
          Nothing

    expressionValueKind =
      expressionValueKindWith Set.empty

    expressionValueKindWith visitedGlobals localForms0 valueKinds0 expr0
      | not (isFunctionLikeBackendType (backendExprType expr0)) =
          LowerRuntimeValue
      | otherwise =
          case expr0 of
            BackendVarWithIdentity ty mbIdentity name ->
              fromMaybe (valueKindForType ty) (variableValueKind visitedGlobals localForms0 valueKinds0 mbIdentity name [])
            BackendTyApp ty fun _ ->
              case collectTyApps expr0 of
                (BackendVarWithIdentity _ mbIdentity name, typeArgs) ->
                  fromMaybe (valueKindForType ty) (variableValueKind visitedGlobals localForms0 valueKinds0 mbIdentity name typeArgs)
                _ ->
                  expressionValueKindWith visitedGlobals localForms0 valueKinds0 fun
            BackendLetWithIdentity _ mbIdentity name bindingTy rhs body ->
              expressionValueKindWith visitedGlobals localForms' valueKinds' body
              where
                localForms' = collectLetLocalForm localForms0 mbIdentity name bindingTy rhs
                valueKinds' =
                  case letBoundValueKind visitedGlobals localForms0 valueKinds0 bindingTy rhs of
                    Just kind -> bindLocalValueKind mbIdentity kind valueKinds0
                    Nothing -> deleteLocalValueKind mbIdentity valueKinds0
            BackendCase _ scrutinee alternatives ->
              combineValueKinds
                (backendExprType expr0)
                [ expressionValueKindWith
                    visitedGlobals
                    localForms0
                    (alternativeValueKinds valueKinds0 scrutinee alternative)
                    (backendAltBody alternative)
                | alternative <- NE.toList alternatives
                ]
            BackendClosure _ _ _ _ _ ->
              LowerClosureRecord
            _ ->
              valueKindForType (backendExprType expr0)

    variableValueKind visitedGlobals localForms0 valueKinds0 mbIdentity _name typeArgs =
      case lookupLocalValueKind mbIdentity valueKinds0 of
        Just kind ->
          Just kind
        Nothing
          | Just _ <- lookupLocalFunctionForm mbIdentity localForms0 ->
              Just LowerFunctionPointer
          | otherwise ->
              case lookupNonLocalBindingInfo base mbIdentity of
                Just binding ->
                  case instantiateFunctionFormWithTypeArgs "closure capture classification" (biForm binding) typeArgs [] of
                    Right (_, form)
                      | null (ffParams form),
                        isFunctionLikeBackendType (ffReturnType form) ->
                          Just (nullaryGlobalReturnValueKind visitedGlobals (bindingInfoRef binding) (ffReturnType form) form)
                    _ ->
                      Just LowerFunctionPointer
                Nothing ->
                  Just LowerClosureRecord

    valueKindEnv =
      ProgramEnv
        { peBase = base,
          peSpecializations = Map.empty,
          peEvidenceWrappers = Map.empty,
          peFunctionWrappers = Map.empty,
          peStringGlobals = Map.empty
        }

    nullaryGlobalReturnValueKind visitedGlobals ref returnTy form
      | Set.member ref visitedGlobals =
          valueKindForType returnTy
      | otherwise =
          backendExprValueKindWith valueKindEnv (Set.insert ref visitedGlobals) emptyLocalValueKinds (ffBody form)

    collectAlternativeEntries alternative =
      let binderRefs = patternBinderRefs (backendAltPattern alternative)
       in collectClosureEntriesInExpr
            base
            (shadowLocalFunctionForms (map fst binderRefs) localForms)
            (shadowLocalValueKinds (map fst binderRefs) valueKinds)
            (backendAltBody alternative)

    alternativeValueKinds valueKinds0 scrutinee alternative =
      patternValueKinds (backendExprType scrutinee) (backendAltPattern alternative) `unionLocalValueKinds` valueKinds0

    patternValueKinds scrutineeTy =
      \case
        BackendDefaultPattern ->
          emptyLocalValueKinds
        BackendConstructorPatternWithBinderIdentities mbIdentity constructorName binders ->
          foldr bindPatternField emptyLocalValueKinds (zip binders fieldTys)
          where
            fieldTys =
              fromMaybe [] $
                lookupConstructorRuntime base mbIdentity constructorName >>= \constructorRuntime ->
                  constructorRuntimeFieldTypes constructorRuntime scrutineeTy
            bindPatternField (binder, fieldTy) =
              bindLocalValueKind
                (backendPatternBinderIdentity binder)
                (constructorFieldStoredValueKind fieldTy)

    collectCaseResultAdapterEntries resultTy alternatives0 =
      [ returnedPartialEntry LowerFunctionPointer resultTy [] paramTys returnTy [] resultTy
      | isFirstOrderFunctionPointerType resultTy,
        not (null paramTys),
        resultKind == LowerClosureRecord,
        LowerClosureRecord `elem` branchKinds
      ]
      where
        branchKinds =
          [ expressionValueKind localForms valueKinds (backendAltBody alternative)
          | alternative <- alternatives0
          ]
        resultKind = combineValueKinds resultTy branchKinds
        (paramTys, returnTy) = collectArrowsType resultTy

    collectConstructorFieldAdapterEntries resultTy mbIdentity name args =
      [ returnedPartialEntry LowerFunctionPointer fieldTy [] paramTys returnTy [] fieldTy
      | Just fieldTys <- [lookupConstructorRuntime base mbIdentity name >>= \constructorRuntime -> constructorRuntimeFieldTypes constructorRuntime resultTy],
        fieldTy <- take (length args) fieldTys,
        isFirstOrderFunctionPointerType fieldTy,
        let (paramTys, returnTy) = collectArrowsType fieldTy,
        not (null paramTys)
      ]



closureEntryOwnerName :: String -> [BackendType] -> String
closureEntryOwnerName name typeArgs =
  name ++ concatMap (("$" ++) . backendTypeKey) typeArgs

evidenceWrapperForm :: Wrapper -> FunctionForm
evidenceWrapperForm = wrapperForm evidenceWrapperArgPrefix False

functionWrapperForm :: Wrapper -> FunctionForm
functionWrapperForm = wrapperForm functionWrapperArgPrefix False

evidenceWrapperArgPrefix :: String
evidenceWrapperArgPrefix =
  "__mlfp_evidence_arg"

functionWrapperArgPrefix :: String
functionWrapperArgPrefix =
  "__mlfp_function_arg"

wrapperParamNames :: String -> BackendType -> [String]
wrapperParamNames argPrefix expectedTy =
  take (length params) [argPrefix ++ show index0 | index0 <- [(0 :: Int) ..]]
  where
    (params, _) = collectArrowsType expectedTy

wrapperForm :: String -> Bool -> Wrapper -> FunctionForm
wrapperForm argPrefix paramsAreEvidence wrapper =
  FunctionForm
    { ffTypeBinders = [],
      ffParams = zip paramNames params,
      ffParamIdentities = paramIdentities,
      ffEvidenceParams =
        if paramsAreEvidence
          then Set.fromList [0 .. length params - 1]
          else Set.empty,
      ffBody = body,
      ffReturnType = returnTy
    }
  where
    (params, returnTy) = collectArrowsType (wrapperExpectedType wrapper)
    paramNames = wrapperParamNames argPrefix (wrapperExpectedType wrapper)
    paramIdentities = take (length params) (wrapperParamIdentities wrapper ++ repeat Nothing)
    paramExprs = [BackendVarWithIdentity paramTy mbIdentity name | (mbIdentity, name, paramTy) <- zip3 paramIdentities paramNames params]
    body = applyWrapperArgs (wrapperExpr wrapper) (wrapperExpectedType wrapper) paramExprs

applyWrapperArgs :: BackendExpr -> BackendType -> [BackendExpr] -> BackendExpr
applyWrapperArgs expr _ [] =
  expr
applyWrapperArgs expr ty args
  | backendExprUsesClosureCallPath expr =
      BackendClosureCall
        { backendExprType = returnTy,
          backendClosureFunction = expr,
          backendClosureArguments = args
        }
  where
    (_, returnTy) = collectArrowsType ty
applyWrapperArgs expr ty (arg : rest) =
  case ty of
    BTArrow _ resultTy ->
      applyWrapperArgs (BackendApp resultTy expr arg) resultTy rest
    _ ->
      expr

backendExprUsesClosureCallPath :: BackendExpr -> Bool
backendExprUsesClosureCallPath expr =
  case backendCallableHead (\_ _ -> BackendCallableBindingUnknown) expr of
    BackendClosureCallableHead _ -> True
    _ -> False

lowerFunction :: ProgramEnv -> IdentityGenerator -> BackendBindingRef -> String -> Bool -> FunctionForm -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)
lowerFunction env generator bindingRef name private form = do
  unless (null (ffTypeBinders form)) $
    Left (BackendLLVMUnsupportedExpression ("binding " ++ show name) "unspecialized polymorphic binding")
  returnTy <- lowerRuntimeValueType env ("return type of " ++ name) (ffReturnType form)
  params <- traverse lowerParam (indexed (ffParams form))
  let initialExprEnv = initialFunctionEnv bindingRef form params
  (result, state') <-
    runStateT
      ( do
          bodyValue <- lowerExpr env initialExprEnv ("binding " ++ show name) (ffBody form)
          unless (lvLLVMType bodyValue == returnTy) $
            liftEither (BackendLLVMInternalError ("LLVM return type mismatch in " ++ name))
          finishCurrentBlock (LLVMRet returnTy (lvOperand bodyValue))
          gets (reverse . fsCompletedBlocks)
      )
      (initialFunctionState generator)
  pure
    ( fsIdentityGenerator state',
      LLVMFunction
        { llvmFunctionName = name,
          llvmFunctionPrivate = private,
          llvmFunctionReturnType = returnTy,
          llvmFunctionParameters = params,
          llvmFunctionBlocks = result
        }
    )
  where
    lowerParam (index0, (paramName, paramTy)) = do
      llvmTy <- lowerFunctionParameterType env ("parameter " ++ show paramName ++ " of " ++ name) (ffEvidenceParams form) index0 paramTy
      pure (LLVMParameter llvmTy paramName)

lowerClosureEntry :: ProgramEnv -> ClosureEntry -> IdentityGenerator -> Either BackendLLVMError (IdentityGenerator, LLVMFunction)
lowerClosureEntry env entry generator = do
  returnTy <- lowerBackendType env ("return type of closure " ++ ceEntryName entry) (closureReturnType entry)
  let rawParamTriples = closureEntryParamTriples entry
      paramNames = Set.fromList [name | (identity, name, _) <- rawParamTriples, not (hasLocalIdentity identity)]
      reserved = freeBackendExprVars (ceBody entry) `Set.difference` paramNames
      (paramTriples, renaming) = freshenLambdaParams reserved rawParamTriples
      body = renameBackendVars renaming (ceBody entry)
  params <- traverse lowerParam (indexed [(paramName, paramTy) | (_, paramName, paramTy) <- paramTriples])
  let envParameter = LLVMParameter LLVMPtr "__mlfp_env"
      initialExprEnv =
        foldl'
          bindParam
          emptyExprEnv
          (indexed (zip paramTriples params))
  (result, state') <-
    runStateT
      ( do
          bodyEnv <- loadClosureCaptures initialExprEnv
          bodyValue <- lowerExpr env bodyEnv ("closure " ++ show (ceEntryName entry)) body
          unless (lvLLVMType bodyValue == returnTy) $
            liftEither (BackendLLVMInternalError ("closure return type mismatch in " ++ ceEntryName entry))
          finishCurrentBlock (LLVMRet returnTy (lvOperand bodyValue))
          gets (reverse . fsCompletedBlocks)
      )
      (initialFunctionState generator)
  pure
    ( fsIdentityGenerator state',
      LLVMFunction
        { llvmFunctionName = ceEntryName entry,
          llvmFunctionPrivate = True,
          llvmFunctionReturnType = returnTy,
          llvmFunctionParameters = envParameter : params,
          llvmFunctionBlocks = result
        }
    )
  where
    closureReturnType closureEntry =
      case collectArrowsType (ceFunctionType closureEntry) of
        (_, returnTy) -> returnTy

    lowerParam (index0, (paramName, paramTy)) = do
      llvmTy <- lowerArgumentType env ("closure parameter " ++ show paramName ++ " of " ++ ceEntryName entry) (ceEvidenceParams entry) index0 paramTy
      pure (LLVMParameter llvmTy paramName)

    bindParam exprEnv (index0, ((mbIdentity, paramName, paramTy), param)) =
      bindExprEnvValue
        mbIdentity
        ( LowerValue
            paramTy
            (llvmParameterType param)
            (LLVMLocal (llvmParameterType param) paramName)
            (parameterValueKind (ceEvidenceParams entry) index0 paramTy)
            Nothing
        )
        exprEnv

    loadClosureCaptures exprEnv0 =
      foldM loadOne exprEnv0 (zip [0 :: Int ..] (ceCaptures entry))

    loadOne exprEnv0 (index0, capture) = do
      let captureName = ccsName capture
          captureTy = ccsType capture
      llvmTy <- lowerClosureStoredTypeM env ("closure capture " ++ show captureName ++ " of " ++ ceEntryName entry) captureTy
      fieldPtr <- emitGep "closure.env.field.ptr" (LLVMLocal LLVMPtr "__mlfp_env") (8 * index0)
      loaded <- emitAssign "closure.env.field" llvmTy (LLVMLoad llvmTy fieldPtr)
      pure $
        bindExprEnvValue
          (ccsIdentity capture)
          (LowerValue captureTy llvmTy loaded (ccsValueKind capture) Nothing)
          exprEnv0

lowerFunctionParameterType :: ProgramEnv -> String -> Set Int -> Int -> BackendType -> Either BackendLLVMError LLVMType
lowerFunctionParameterType env context evidenceParams index0 paramTy
  | isEvidenceParameter evidenceParams index0 paramTy || isFirstOrderFunctionPointerType paramTy = Right LLVMPtr
  | otherwise = lowerBackendType env context paramTy

lowerFunctionParameterTypeM :: ProgramEnv -> String -> Set Int -> Int -> BackendType -> LowerM LLVMType
lowerFunctionParameterTypeM env context evidenceParams index0 paramTy =
  case lowerArgumentType env context evidenceParams index0 paramTy of
    Right llvmTy -> pure llvmTy
    Left err -> liftEither err

lowerArgumentType :: ProgramEnv -> String -> Set Int -> Int -> BackendType -> Either BackendLLVMError LLVMType
lowerArgumentType env context evidenceParams index0 paramTy
  | isEvidenceArgument evidenceParams index0 paramTy = Right LLVMPtr
  | isFirstOrderFunctionPointerType paramTy = Right LLVMPtr
  | otherwise = lowerBackendType env context paramTy

isEvidenceArgument :: Set Int -> Int -> BackendType -> Bool
isEvidenceArgument evidenceParams index0 paramTy =
  Set.member index0 evidenceParams && isFunctionLikeBackendType paramTy

isEvidenceParameter :: Set Int -> Int -> BackendType -> Bool
isEvidenceParameter =
  isEvidenceArgument

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
  any (\(index0, (_, paramTy)) -> isInlineOnlyFunctionParameter (ffEvidenceParams form) index0 paramTy) (indexed (ffParams form))
    || containsInlineOnlyEvidenceParameterCall form

containsInlineOnlyEvidenceParameterCall :: FunctionForm -> Bool
containsInlineOnlyEvidenceParameterCall form =
  go (evidenceParameterKeys form) Set.empty (ffBody form)
  where
    go evidenceParams localFunctions expr =
      callRequiresInline evidenceParams localFunctions expr
        || case expr of
          BackendVarWithIdentity {} -> False
          BackendLit {} -> False
          BackendLamWithIdentity _ mbIdentity _name _ body ->
            go evidenceParams (localFunctions `Set.difference` (termBoundKeys mbIdentity)) body
          BackendApp _ fun arg ->
            go evidenceParams localFunctions fun || go evidenceParams localFunctions arg
          BackendLetWithIdentity _ mbIdentity _name bindingTy rhs body ->
            let rhsForm = functionFormFromExpected bindingTy rhs
                rhsIsLocalFunction = not (null (ffTypeBinders rhsForm)) || not (null (ffParams rhsForm))
                localFunctions' =
                  if rhsIsLocalFunction
                    then Set.union (termBoundKeys mbIdentity) localFunctions
                    else localFunctions `Set.difference` (termBoundKeys mbIdentity)
             in go evidenceParams localFunctions rhs || go evidenceParams localFunctions' body
          BackendTyAbs _ _ _ body ->
            go evidenceParams localFunctions body
          BackendTyApp _ fun _ ->
            go evidenceParams localFunctions fun
          BackendConstructWithIdentity _ _ _ args ->
            any (go evidenceParams localFunctions) args
          BackendCase _ scrutinee alternatives ->
            go evidenceParams localFunctions scrutinee
              || any (goAlternative evidenceParams localFunctions) (NE.toList alternatives)
          BackendRoll _ payload ->
            go evidenceParams localFunctions payload
          BackendUnroll _ payload ->
            go evidenceParams localFunctions payload
          BackendClosureWithParamIdentities _ _ _ captures params body ->
            any (go evidenceParams localFunctions . backendClosureCaptureExpr) captures
              || go
                evidenceParams
                (localFunctions `Set.difference` (termBoundKeyRefs (map fst closureRefs)))
                body
            where
              closureRefs =
                [(backendClosureCaptureIdentity capture, backendClosureCaptureName capture) | capture <- captures]
                  ++ [(backendClosureParamIdentity param, backendClosureParamName param) | param <- params]
          BackendClosureCall _ fun args ->
            go evidenceParams localFunctions fun || any (go evidenceParams localFunctions) args

    goAlternative evidenceParams localFunctions (BackendAlternative pattern0 body) =
      go evidenceParams (localFunctions `Set.difference` patternTermBoundKeys pattern0) body

    callRequiresInline evidenceParams localFunctions expr =
      case collectCall expr of
        Just (BackendVarWithIdentity calleeTy mbIdentity _name, typeArgs, args)
          | not (Set.null (termReferenceKeys mbIdentity `Set.intersection` evidenceParams)) ->
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
        (BackendVarWithIdentity _ mbIdentity _name, _) ->
          not (Set.null (termReferenceKeys mbIdentity `Set.intersection` localFunctions))
        _ ->
          case arg of
            BackendLam _ _ _ _ -> True
            BackendTyAbs _ _ _ _ -> True
            BackendLetWithIdentity _ mbIdentity _name bindingTy rhs body ->
              let rhsForm = functionFormFromExpected bindingTy rhs
                  localFunctions' =
                    if not (null (ffTypeBinders rhsForm)) || not (null (ffParams rhsForm))
                      then Set.union (termBoundKeys mbIdentity) localFunctions
                      else localFunctions `Set.difference` (termBoundKeys mbIdentity)
               in functionExpressionRequiresInline localFunctions' body
            _ -> False

evidenceParameterKeys :: FunctionForm -> Set TermBoundKey
evidenceParameterKeys form =
  Set.unions
    [ termBoundKeys mbIdentity
    | (index0, (mbIdentity, _name, ty)) <- indexed (functionFormParamTriples form),
      isEvidenceArgument (ffEvidenceParams form) index0 ty
    ]

hasTypeBinders :: BackendType -> Bool
hasTypeBinders =
  \case
    BTForall {} -> True
    _ -> False

initialFunctionState :: IdentityGenerator -> FunctionState
initialFunctionState generator =
  FunctionState
    { fsNextLocal = 0,
      fsNextBlock = 0,
      fsIdentityGenerator = generator,
      fsCurrentLabel = "entry",
      fsCurrentInstructions = [],
      fsCompletedBlocks = []
    }

initialFunctionEnv :: BackendBindingRef -> FunctionForm -> [LLVMParameter] -> ExprEnv
initialFunctionEnv bindingRef form params =
  foldl'
    bindParam
    (emptyExprEnv {eeActiveGlobalInlines = Set.singleton bindingRef})
    (indexed (zip (functionFormParamTriples form) params))
  where
    bindParam exprEnv (index0, ((mbIdentity, paramName, paramTy), param)) =
      bindExprEnvValue
        mbIdentity
        ( LowerValue
            paramTy
            (llvmParameterType param)
            (LLVMLocal (llvmParameterType param) paramName)
            (parameterValueKind (ffEvidenceParams form) index0 paramTy)
            Nothing
        )
        exprEnv

liftEither :: BackendLLVMError -> LowerM a
liftEither =
  StateT . const . Left

lowerExpr :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerExpr env exprEnv context expr =
  case expr of
    BackendVarWithIdentity ty mbIdentity name ->
      lowerVar env exprEnv context ty mbIdentity name
    BackendLit ty lit ->
      lowerLit env context ty lit
    BackendLamWithIdentity {} ->
      liftEither (BackendLLVMUnsupportedExpression context "escaping lambda")
    BackendApp {} ->
      lowerCall env exprEnv context expr
    BackendLetWithIdentity resultTy mbIdentity name _ rhs body -> do
      exprEnv' <- bindLet env exprEnv context mbIdentity name rhs
      bodyValue <- lowerExpr env exprEnv' context body
      expectedTy <- lowerRuntimeValueTypeM env context resultTy
      unless (lvLLVMType bodyValue == expectedTy) $
        liftEither (BackendLLVMInternalError ("let result type mismatch at " ++ context))
      pure bodyValue
    BackendTyAbsWithIdentity {} ->
      liftEither (BackendLLVMUnsupportedExpression context "escaping type abstraction")
    BackendTyApp {} ->
      lowerTyApp env exprEnv context expr
    BackendConstructWithIdentity resultTy mbIdentity name args ->
      lowerConstruct env exprEnv context resultTy mbIdentity name args
    BackendCase resultTy scrutinee alternatives ->
      lowerCase env exprEnv context resultTy scrutinee alternatives
    BackendRoll resultTy payload ->
      lowerRollLike env exprEnv context resultTy payload "roll"
    BackendUnroll resultTy payload ->
      lowerRollLike env exprEnv context resultTy payload "unroll"
    BackendClosureWithParamIdentities resultTy _ entryName captures _ _ ->
      lowerClosureValue env exprEnv context resultTy entryName captures
    BackendClosureCall resultTy fun args ->
      lowerClosureCall env exprEnv context resultTy fun args

lowerTyApp :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerTyApp env exprEnv context expr =
  case collectTyApps expr of
    (BackendVarWithIdentity _ mbIdentity name, typeArgs)
      | Just localFunction <- lookupExprEnvLocalFunction mbIdentity exprEnv ->
          lowerLocalFunctionValue env context (backendExprType expr) name localFunction typeArgs
      | Just binding <- lookupNonLocalBindingInfo (peBase env) mbIdentity,
        not (null (ffTypeBinders (biForm binding))) ->
          lowerGlobalValue env exprEnv context (backendExprType expr) (biName binding) binding typeArgs
      | Just primitiveName <- ioPrimitiveRuntimeName mbIdentity name ->
          resolveIOPrimitiveAsValue (backendExprType expr) primitiveName
      | Just primitiveName <- nativePrimitiveRuntimeName mbIdentity name ->
          resolveNativePrimitiveAsValue (backendExprType expr) primitiveName
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
    BackendLetWithIdentity _ mbIdentity name bindingTy rhs body -> do
      appliedBody <- applyTypeApplicationsToExpr context resultTy body typeArgs
      pure (Just (BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs appliedBody))
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
        BTForallWithIdentity identity name _ bodyTy ->
          let resultTy = substituteBackendTypeForBinder identity name typeArg bodyTy
           in Right (BackendTyApp resultTy current typeArg, resultTy)
        _ ->
          Left (BackendLLVMUnsupportedCall ("unexpected type arguments at " ++ context))

pushCallIntoExpression :: String -> BackendType -> BackendExpr -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError (Maybe BackendExpr)
pushCallIntoExpression context resultTy fun typeArgs args =
  case fun of
    BackendLetWithIdentity _ mbIdentity name bindingTy rhs body -> do
      appliedBody <- applyCallToExpr context resultTy body typeArgs args
      pure (Just (BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs appliedBody))
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
  case backendCallableHead (\_ _ -> BackendCallableBindingUnknown) typedExpr of
    BackendClosureCallableHead ref ->
      Left (BackendLLVMValidationFailed (BackendClosureCalledWithBackendApp (backendCallableRefName ref)))
    _ -> do
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
lowerLocalFunctionValue env context resultTy _name localFunction typeArgs = do
  case residualZeroArityPolymorphism context typeArgs (lfForm localFunction) of
    Just err -> liftEither err
    Nothing -> pure ()
  (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (lfForm localFunction) typeArgs []
  let ownerName = lfName localFunction
      form = qualifyInstantiatedClosureEntries ownerName resolvedTypeArgs form0
  lowerInstantiatedFunctionValue env (lfCapturedEnv localFunction) context ownerName resultTy form

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

bindLet :: ProgramEnv -> ExprEnv -> String -> Maybe IdDetails -> String -> BackendExpr -> LowerM ExprEnv
bindLet env exprEnv context mbIdentity name rhs =
  case callablePointerAliasValue exprEnv rhs of
    Just value ->
      pure (bindExprEnvValue mbIdentity value exprEnv)
    Nothing -> do
      form <- functionFormFromExpectedM (backendExprType rhs) rhs
      if not (null (ffTypeBinders form)) || not (null (ffParams form))
        then
          pure $
            bindExprEnvLocalFunction
              mbIdentity
              LocalFunction
                { lfName = name,
                  lfForm = form,
                  lfCapturedEnv = exprEnv,
                  lfStoredReference = Just (backendExprType rhs, rhs)
                }
              exprEnv
        else do
          value <- lowerExpr env exprEnv (context ++ ", let " ++ show name) rhs
          pure (bindExprEnvValue mbIdentity value exprEnv)

callablePointerAliasValue :: ExprEnv -> BackendExpr -> Maybe LowerValue
callablePointerAliasValue =
  pointerAliasValue isCallablePointerAliasValue
  where
    isCallablePointerAliasValue value =
      case lvValueKind value of
        LowerClosureRecord -> True
        LowerFunctionPointer -> True
        LowerRuntimeValue -> False

closurePointerAliasValue :: ExprEnv -> BackendExpr -> Maybe LowerValue
closurePointerAliasValue =
  pointerAliasValue ((== LowerClosureRecord) . lvValueKind)

pointerAliasValue :: (LowerValue -> Bool) -> ExprEnv -> BackendExpr -> Maybe LowerValue
pointerAliasValue matches exprEnv =
  \case
    BackendVarWithIdentity ty mbIdentity _name
      | isFunctionLikeBackendType ty,
        Just value <- lookupExprEnvValue mbIdentity exprEnv,
        lvLLVMType value == LLVMPtr,
        matches value ->
          Just value {lvBackendType = ty}
    BackendTyApp ty fun _
      | isFunctionLikeBackendType ty,
        Just value <- pointerAliasValue matches exprEnv fun ->
          Just value {lvBackendType = ty}
    BackendLetWithIdentity ty mbIdentity _name bindingTy rhs body
      | isFunctionLikeBackendType ty ->
          let exprEnvForBody =
                case pointerAliasValue matches exprEnv rhs of
                  Just value ->
                    bindExprEnvValue mbIdentity (value {lvBackendType = bindingTy}) exprEnv
                  Nothing ->
                    deleteExprEnvBinding mbIdentity exprEnv
           in pointerAliasValue matches exprEnvForBody body
    _ ->
      Nothing

lowerVar :: ProgramEnv -> ExprEnv -> String -> BackendType -> Maybe IdDetails -> String -> LowerM LowerValue
lowerVar env exprEnv context ty mbIdentity name =
  case lookupExprEnvValue mbIdentity exprEnv of
    Just value -> pure value
    Nothing ->
      case lookupExprEnvLocalFunction mbIdentity exprEnv of
        Just localFunction ->
          lowerLocalFunctionValue env context ty name localFunction []
        Nothing ->
          case lookupNonLocalBindingInfo (peBase env) mbIdentity of
            Just binding ->
              lowerGlobalValue env exprEnv context ty (biName binding) binding []
            Nothing
              | Just primitiveName <- ioPrimitiveRuntimeName mbIdentity name ->
                  resolveIOPrimitiveAsValue ty primitiveName
            Nothing
              | Just primitiveName <- nativePrimitiveRuntimeName mbIdentity name ->
                  resolveNativePrimitiveAsValue ty primitiveName
            Nothing ->
              liftEither (BackendLLVMUnknownFunction name)

lowerGlobalValue :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> BindingInfo -> [BackendType] -> LowerM LowerValue
lowerGlobalValue env exprEnv context resultTy name binding typeArgs =
  case (ffTypeBinders form, typeArgs) of
    ([], []) ->
      lowerInstantiatedGlobalValue resultTy name binding [] form
    ([], _ : _) ->
      liftEither (BackendLLVMUnsupportedCall ("unexpected type arguments at " ++ context))
    (_ : _, []) ->
      case residualZeroArityPolymorphism context typeArgs form of
        Just err ->
          liftEither err
        Nothing ->
          liftEither (BackendLLVMUnsupportedExpression context ("escaping polymorphic binding " ++ show name))
    (_ : _, _) -> do
      (resolvedTypeArgs, instantiated) <- instantiateFunctionFormWithTypeArgsM context form typeArgs []
      lowerInstantiatedGlobalValue resultTy name binding resolvedTypeArgs instantiated
  where
    form = biForm binding

    lowerInstantiatedGlobalValue expectedTy functionContext binding0 resolvedTypeArgs instantiated
      | not (null (ffParams instantiated)) =
          lowerInstantiatedGlobalFunctionValue expectedTy functionContext binding0 resolvedTypeArgs instantiated
      | otherwise =
          if zeroArityGlobalReturnTypeMatches env expectedTy (ffReturnType instantiated)
            then do
              resultLLVMType <- lowerRuntimeValueTypeM env context expectedTy
              functionName <- globalFunctionName env context binding0 resolvedTypeArgs
              result <- emitAssign "call" resultLLVMType (LLVMCall functionName [])
              pure
                ( LowerValue
                    expectedTy
                    resultLLVMType
                    result
                    (functionFormReturnValueKind env instantiated)
                    (functionFormReturnConstructedValue env instantiated)
                )
            -- Fallback: type mismatch between mu-encoded and named types (e.g. BTMu vs BTCon for List).
            -- Safe to inline when body is a simple reference — avoids a spurious function call.
            else case ffBody instantiated of
              BackendVarWithIdentity {} -> lowerExpr env exprEnv context (ffBody instantiated)
              _ -> liftEither (BackendLLVMInternalError ("type mismatch for zero-arity binding at " ++ context ++ ": expected " ++ show expectedTy ++ " but got " ++ show (ffReturnType instantiated)))

    lowerInstantiatedGlobalFunctionValue expectedTy functionContext binding0 resolvedTypeArgs instantiated = do
      unless (isFirstOrderFunctionPointerType expectedTy) $
        liftEither (BackendLLVMUnsupportedExpression context ("escaping function " ++ show functionContext))
      let actualTy = functionTypeFromForm instantiated
      requireEvidenceFunctionType context functionContext expectedTy actualTy
      (functionRef, functionName) <- globalFunctionTarget env context binding0 resolvedTypeArgs
      pure (functionPointerValueForGlobalTarget expectedTy binding0 resolvedTypeArgs functionRef (LLVMGlobalRef LLVMPtr functionName))

zeroArityGlobalReturnTypeMatches :: ProgramEnv -> BackendType -> BackendType -> Bool
zeroArityGlobalReturnTypeMatches env expected actual =
  alphaEqBackendType expected actual || sameRuntimeDataType (peBase env) expected actual

sameRuntimeDataType :: ProgramBase -> BackendType -> BackendType -> Bool
sameRuntimeDataType base left right =
  case (nativeDataRuntimeForType base left, nativeDataRuntimeForType base right) of
    (Just leftRuntime, Just rightRuntime) ->
      backendDataIdentity (drData leftRuntime) == backendDataIdentity (drData rightRuntime)
    _ ->
      False

residualZeroArityPolymorphism :: String -> [BackendType] -> FunctionForm -> Maybe BackendLLVMError
residualZeroArityPolymorphism context typeArgs form
  | null typeArgs,
    not (null (ffTypeBinders form)),
    null (ffParams form) =
      Just (BackendLLVMUnsupportedExpression context "unspecialized polymorphic binding")
  | otherwise =
      Nothing

lowerLit :: ProgramEnv -> String -> BackendType -> Lit -> LowerM LowerValue
lowerLit env context ty lit = do
  llvmTy <- lowerBackendTypeM env context ty
  case lit of
    LInt value ->
      pure (LowerValue ty llvmTy (LLVMIntLiteral 64 value) LowerRuntimeValue Nothing)
    LBool value ->
      pure (LowerValue ty llvmTy (LLVMIntLiteral 1 (if value then 1 else 0)) LowerRuntimeValue Nothing)
    LChar value ->
      pure (LowerValue ty llvmTy (LLVMIntLiteral 32 (toInteger (ord value))) LowerRuntimeValue Nothing)
    LString value ->
      case Map.lookup value (peStringGlobals env) of
        Just globalName
          | nativeStringLiteralSupported value ->
              pure (LowerValue ty llvmTy (LLVMGlobalRef LLVMPtr globalName) LowerRuntimeValue Nothing)
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
  pure (LowerValue resultTy LLVMPtr closurePointer LowerClosureRecord Nothing)
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
        && not (captureExprIsRuntimeClosureValue capture)
        && case collectTyApps (backendClosureCaptureExpr capture) of
          (BackendVarWithIdentity {}, _) -> True
          _ -> False

    captureExprIsRuntimeClosureValue capture =
      case closurePointerAliasValue exprEnv (backendClosureCaptureExpr capture) of
        Just _ ->
          True
        Nothing ->
          captureExprNamesGlobalClosureValue capture

    captureExprNamesGlobalClosureValue capture =
      case collectTyApps (backendClosureCaptureExpr capture) of
        (BackendVarWithIdentity _ mbIdentity _name, typeArgs)
          | Just binding <- lookupNonLocalBindingInfo (peBase env) mbIdentity,
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
    (BackendVarWithIdentity _ mbIdentity name, typeArgs)
      | Just localFunction <- lookupExprEnvLocalFunction mbIdentity exprEnv ->
          lowerLocalFunctionCall env exprEnv context resultTy name localFunction typeArgs args
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
  pure (lowerValueForType resultTy resultLLVMType result)
  where
    lowerClosureArg (index0, paramTy) arg =
      lowerExprForIndirectArgument env exprEnv context Set.empty (index0, ("__mlfp_closure_arg" ++ show index0, paramTy)) arg

lowerClosureValueCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> LowerValue -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerClosureValueCall env exprEnv context resultTy callee typeArgs args = do
  calleeTy <-
    if null typeArgs
      then pure (lvBackendType callee)
      else instantiateCallableTypeM context (lvBackendType callee) typeArgs []
  lowerClosurePointerValueCall env exprEnv context resultTy callee {lvBackendType = calleeTy} args

lowerReturnedFunctionValueCall :: ProgramEnv -> ExprEnv -> String -> String -> BackendType -> LowerValue -> [BackendExpr] -> LowerM LowerValue
lowerReturnedFunctionValueCall env exprEnv context name resultTy callee args = do
  when (lvValueKind callee == LowerRuntimeValue) $
    liftEither (BackendLLVMUnsupportedExpression context ("returned value is not callable: " ++ show (lvBackendType callee)))
  let (paramTys, returnTy) = collectArrowsType (lvBackendType callee)
  when (null paramTys) $
    liftEither (BackendLLVMUnsupportedExpression context ("returned value is not callable: " ++ show (lvBackendType callee)))
  case compare (length args) (length paramTys) of
    LT
      | null args -> do
          unless (alphaEqBackendType resultTy (lvBackendType callee)) $
            liftEither (BackendLLVMInternalError ("returned function value type mismatch at " ++ context))
          pure callee {lvBackendType = resultTy}
      | otherwise ->
          lowerReturnedPartialClosureValue env exprEnv context resultTy callee paramTys returnTy args
    EQ ->
      lowerSaturatedReturnedFunctionValueCall env exprEnv context name resultTy callee args
    GT
      | isFunctionLikeBackendType returnTy -> do
          let (directArgs, remainingArgs) = splitAt (length paramTys) args
          saturated <- lowerSaturatedReturnedFunctionValueCall env exprEnv context name returnTy callee directArgs
          lowerReturnedFunctionValueCall env exprEnv context name resultTy saturated remainingArgs
      | otherwise ->
          liftEither (BackendLLVMArityMismatch name (length paramTys) (length args))

lowerSaturatedReturnedFunctionValueCall :: ProgramEnv -> ExprEnv -> String -> String -> BackendType -> LowerValue -> [BackendExpr] -> LowerM LowerValue
lowerSaturatedReturnedFunctionValueCall env exprEnv context name resultTy callee args =
  case lvValueKind callee of
    LowerClosureRecord ->
      lowerClosurePointerValueCall env exprEnv context resultTy callee args
    LowerFunctionPointer ->
      lowerIndirectValueCall env exprEnv context name callee [] args
    LowerRuntimeValue ->
      liftEither (BackendLLVMUnsupportedExpression context ("returned value is not callable: " ++ show (lvBackendType callee)))

lowerReturnedPartialClosureValue :: ProgramEnv -> ExprEnv -> String -> BackendType -> LowerValue -> [BackendType] -> BackendType -> [BackendExpr] -> LowerM LowerValue
lowerReturnedPartialClosureValue env exprEnv context resultTy callee paramTys returnTy args = do
  unless (lvLLVMType callee == LLVMPtr) $
    liftEither (BackendLLVMUnsupportedExpression context ("returned partial callee is not a pointer: " ++ show (lvBackendType callee)))
  let suppliedCount = length args
      suppliedParamTys = take suppliedCount paramTys
      remainingParamTys = drop suppliedCount paramTys
      expectedResultTy = functionTypeFromParts remainingParamTys returnTy
  unless (suppliedCount < length paramTys) $
    liftEither (BackendLLVMArityMismatch "returned partial" (length paramTys) suppliedCount)
  unless (alphaEqBackendType resultTy expectedResultTy) $
    liftEither (BackendLLVMInternalError ("returned partial result mismatch at " ++ context))
  suppliedValues <- zipWithM lowerPartialArg (zip [0 :: Int ..] suppliedParamTys) args
  let suppliedKinds = map lvValueKind suppliedValues
      entryName = returnedPartialClosureEntryName (lvValueKind callee) (lvBackendType callee) suppliedCount suppliedKinds resultTy
  envPointer <- lowerReturnedPartialEnvironment suppliedParamTys suppliedValues
  closurePointer <- emitMalloc env context 16
  codePtrField <- emitGep "closure.code.ptr" closurePointer 0
  emitStore LLVMPtr (LLVMGlobalRef LLVMPtr entryName) codePtrField
  envPtrField <- emitGep "closure.env.ptr" closurePointer 8
  emitStore LLVMPtr envPointer envPtrField
  pure (LowerValue resultTy LLVMPtr closurePointer LowerClosureRecord Nothing)
  where
    lowerPartialArg (index0, paramTy) arg =
      lowerExprForIndirectArgument env exprEnv context Set.empty (index0, (returnedPartialSuppliedArgName index0, paramTy)) arg

    lowerReturnedPartialEnvironment suppliedParamTys suppliedValues = do
      envPointer <- emitMalloc env context (8 * (1 + length suppliedValues))
      storeCapture envPointer 0 (lvBackendType callee, callee)
      zipWithM_ (storeCapture envPointer) [1 :: Int ..] (zip suppliedParamTys suppliedValues)
      pure envPointer

    storeCapture envPointer index0 (captureTy, value) = do
      expectedTy <- lowerClosureStoredTypeM env context captureTy
      requireLLVMType context returnedPartialCalleeCaptureName expectedTy value
      fieldPtr <- emitGep "closure.env.field.ptr" envPointer (8 * index0)
      emitStore expectedTy (lvOperand value) fieldPtr

lowerClosureCallee :: ProgramEnv -> ExprEnv -> String -> BackendExpr -> LowerM LowerValue
lowerClosureCallee env exprEnv context =
  \case
    BackendLetWithIdentity _ mbIdentity name _ rhs body -> do
      exprEnv' <- bindLet env exprEnv context mbIdentity name rhs
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
        BackendVarWithIdentity _ mbIdentity name ->
          case lookupExprEnvLocalFunction mbIdentity exprEnv of
            Just localFunction ->
              lowerLocalFunctionCall env exprEnv context (backendExprType expr) name localFunction typeArgs args
            Nothing ->
              case lookupExprEnvValue mbIdentity exprEnv of
                Just value
                  | isFunctionLikeBackendType (lvBackendType value),
                    lvValueKind value == LowerClosureRecord ->
                      lowerClosureValueCall env exprEnv context (backendExprType expr) value typeArgs args
                Just value
                  | isFunctionLikeBackendType (lvBackendType value) ->
                      lowerIndirectValueCall env exprEnv context name value typeArgs args
                _ ->
                  lowerGlobalCall env exprEnv context (backendExprType expr) mbIdentity name typeArgs args
        BackendLam _ _ _ _ ->
          lowerDirectFunctionCall env exprEnv context (backendExprType expr) (functionFormFromExpr headExpr) typeArgs args
        BackendTyAbs _ _ _ _ ->
          lowerDirectFunctionCall env exprEnv context (backendExprType expr) (functionFormFromExpr headExpr) typeArgs args
        _ ->
          case backendCallableHead (\_ _ -> BackendCallableBindingUnknown) headExpr of
            BackendClosureCallableHead ref ->
              liftEither (BackendLLVMValidationFailed (BackendClosureCalledWithBackendApp (backendCallableRefName ref)))
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
  unless (lvValueKind callee == LowerFunctionPointer || isFirstOrderFunctionPointerType (lvBackendType callee)) $
    liftEither (BackendLLVMUnsupportedExpression context ("escaping function value " ++ show name))
  form <- instantiateFunctionFormM context (functionFormFromType (lvBackendType callee)) typeArgs args
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  case indirectCalleeFunctionForm env callee of
    Just calleeForm0 -> do
      instantiatedCalleeForm <- instantiateFunctionFormM context calleeForm0 typeArgs args
      if requiresInlineCall instantiatedCalleeForm
        then do
          bodyEnv <- bindCallArguments env exprEnv exprEnv context name instantiatedCalleeForm args
          lowerExpr env bodyEnv context (ffBody instantiatedCalleeForm)
        else lowerIndirectPointerCall form
    Nothing ->
      lowerIndirectPointerCall form
  where
    lowerIndirectPointerCall form = do
      callArgs <- zipWithM (lowerExprForIndirectArgument env exprEnv context (ffEvidenceParams form)) (indexed (ffParams form)) args
      bindIndirectFunctionArguments env context name form callArgs
      resultTy <- lowerRuntimeValueTypeM env context (ffReturnType form)
      result <- emitAssign "call" resultTy (LLVMCallOperand (lvOperand callee) [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
      pure (lowerValueForType (ffReturnType form) resultTy result)

indirectCalleeFunctionForm :: ProgramEnv -> LowerValue -> Maybe FunctionForm
indirectCalleeFunctionForm env callee =
  lookupFunctionFormByRef env (lvBindingRef callee) <|> lookupFunctionFormByIdentity env (lvSymbolIdentity callee)

lookupFunctionFormByRef :: ProgramEnv -> Maybe BackendBindingRef -> Maybe FunctionForm
lookupFunctionFormByRef env mbRef =
  mbRef >>= \ref ->
    (biForm <$> lookupBindingRef (peBase env) ref)
      <|> (qualifiedSpecializationForm <$> find ((== ref) . spBindingRef) (Map.elems (peSpecializations env)))
      <|> (qualifiedEvidenceWrapperForm <$> find ((== ref) . wrapperBindingRef) (Map.elems (peEvidenceWrappers env)))
      <|> (qualifiedFunctionWrapperForm <$> find ((== ref) . wrapperBindingRef) (Map.elems (peFunctionWrappers env)))

lookupFunctionFormByIdentity :: ProgramEnv -> Maybe SymbolIdentity -> Maybe FunctionForm
lookupFunctionFormByIdentity env mbIdentity =
  biForm <$> (mbIdentity >>= (`Map.lookup` pbBindingsByIdentity (peBase env)))

functionFormFromType :: BackendType -> FunctionForm
functionFormFromType ty =
  FunctionForm
    { ffTypeBinders = typeBinders,
      ffParams = zip paramNames params,
      ffParamIdentities = identities,
      ffEvidenceParams = Set.empty,
      ffBody = BackendVar returnTy "__mlfp_callable_result",
      ffReturnType = returnTy
    }
  where
    (typeBinders, afterForalls) = collectForallsType ty
    (params, returnTy) = collectArrowsType afterForalls
    paramNames = ["__mlfp_callable_arg" ++ show index0 | index0 <- [(0 :: Int) ..]]
    (_, identities) =
      generatedLocalIdentities
        (identityGeneratorAfter (generatedIdentitiesInBackendTypes [ty]))
        (take (length params) paramNames)

lowerExprForArgument :: ProgramEnv -> ExprEnv -> String -> Set Int -> (Int, (String, BackendType)) -> BackendExpr -> LowerM LowerValue
lowerExprForArgument env exprEnv context evidenceParams (index0, (_, paramTy)) arg
  | isEvidenceArgument evidenceParams index0 paramTy =
      lowerEvidenceArgument env exprEnv context paramTy arg
  | isFirstOrderFunctionPointerType paramTy =
      lowerFunctionArgument env exprEnv context paramTy arg
  | otherwise =
      lowerExpr env exprEnv context arg

lowerExprForIndirectArgument :: ProgramEnv -> ExprEnv -> String -> Set Int -> (Int, (String, BackendType)) -> BackendExpr -> LowerM LowerValue
lowerExprForIndirectArgument env exprEnv context evidenceParams (index0, (_, paramTy)) arg
  | isEvidenceArgument evidenceParams index0 paramTy =
      lowerEvidenceArgument env exprEnv context paramTy arg
  | isFunctionLikeBackendType paramTy =
      case Map.lookup (wrapperKey' paramTy arg) (peEvidenceWrappers env) of
        Just wrapper ->
          pure (functionPointerValueForBindingRef paramTy (wrapperBindingRef wrapper) (LLVMGlobalRef LLVMPtr (wrapperFunctionName wrapper)))
        Nothing ->
          lowerFunctionArgument env exprEnv context paramTy arg
  | otherwise =
      lowerExpr env exprEnv context arg

lowerFunctionArgument :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerFunctionArgument env exprEnv context expectedTy arg =
  case collectTyApps arg of
    (BackendVarWithIdentity _ mbIdentity name, typeArgs) ->
      lowerFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs
    _ ->
      liftEither (BackendLLVMUnsupportedExpression context "unsupported function argument")

lowerStoredFunctionArgument :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerStoredFunctionArgument env exprEnv context expectedTy arg =
  case collectTyApps arg of
    (BackendVarWithIdentity _ mbIdentity name, typeArgs) ->
      lowerStoredFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs
    _ ->
      case Map.lookup (wrapperKey' expectedTy arg) (peFunctionWrappers env) of
        Just wrapper ->
          pure (functionPointerValueForBindingRef expectedTy (wrapperBindingRef wrapper) (LLVMGlobalRef LLVMPtr (wrapperFunctionName wrapper)))
        Nothing ->
          liftEither (BackendLLVMUnsupportedExpression context "unsupported function argument")

lowerEvidenceArgument :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerEvidenceArgument env exprEnv context expectedTy arg =
  case collectTyApps arg of
    (BackendVarWithIdentity _ mbIdentity name, typeArgs) ->
      lowerFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs
    _ ->
      case Map.lookup (wrapperKey' expectedTy arg) (peEvidenceWrappers env) of
        Just wrapper ->
          pure (functionPointerValueForBindingRef expectedTy (wrapperBindingRef wrapper) (LLVMGlobalRef LLVMPtr (wrapperFunctionName wrapper)))
        Nothing ->
          liftEither (BackendLLVMUnsupportedExpression context "unsupported evidence function argument")

lowerFunctionReference :: ProgramEnv -> ExprEnv -> String -> BackendType -> Maybe IdDetails -> String -> [BackendType] -> LowerM LowerValue
lowerFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs =
  case lookupExprEnvLocalFunction mbIdentity exprEnv of
    Just localFunction ->
      lowerLocalFunctionReference env context expectedTy name localFunction typeArgs
    Nothing ->
      lowerNonLocalFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs

lowerStoredFunctionReference :: ProgramEnv -> ExprEnv -> String -> BackendType -> Maybe IdDetails -> String -> [BackendType] -> LowerM LowerValue
lowerStoredFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs =
  case lookupExprEnvLocalFunction mbIdentity exprEnv of
    Just localFunction ->
      lowerLocalFunctionReferenceWith True env context expectedTy name localFunction typeArgs
    Nothing ->
      lowerNonLocalFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs

lowerLocalFunctionReference :: ProgramEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> LowerM LowerValue
lowerLocalFunctionReference =
  lowerLocalFunctionReferenceWith False

lowerLocalFunctionReferenceWith :: Bool -> ProgramEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> LowerM LowerValue
lowerLocalFunctionReferenceWith allowStoredReference env context expectedTy _name localFunction typeArgs = do
  (resolvedTypeArgs, form0) <-
    if null typeArgs
      then pure ([], lfForm localFunction)
      else instantiateFunctionFormWithTypeArgsM context (lfForm localFunction) typeArgs []
  let ownerName = lfName localFunction
      form = qualifyInstantiatedClosureEntries ownerName resolvedTypeArgs form0
  let actualTy = functionTypeFromFormWithBinders form
  requireEvidenceFunctionType context ownerName expectedTy actualTy
  case etaAliasTarget form of
    Just (targetIdentity, targetName, targetTypeArgs) ->
      if allowStoredReference
        then lowerStoredFunctionReference env (lfCapturedEnv localFunction) context expectedTy targetIdentity targetName targetTypeArgs
        else lowerFunctionReference env (lfCapturedEnv localFunction) context expectedTy targetIdentity targetName targetTypeArgs
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
      liftEither (BackendLLVMUnsupportedExpression context ("unsupported function argument " ++ show (lfName localFunction)))

lowerLocalFunctionStoredReference :: ProgramEnv -> String -> BackendType -> LocalFunction -> [BackendType] -> LowerM (Maybe LowerValue)
lowerLocalFunctionStoredReference env context expectedTy localFunction typeArgs =
  case lfStoredReference localFunction of
    Just (_, sourceExpr0) -> do
      sourceExpr <- storedReferenceSourceExpr context sourceExpr0 typeArgs
      case Map.lookup (wrapperKey' expectedTy sourceExpr) (peFunctionWrappers env) of
        Just wrapper ->
          pure (Just (functionPointerValueForBindingRef expectedTy (wrapperBindingRef wrapper) (LLVMGlobalRef LLVMPtr (wrapperFunctionName wrapper))))
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

etaAliasTarget :: FunctionForm -> Maybe (Maybe IdDetails, String, [BackendType])
etaAliasTarget form =
  case collectValueApps (ffBody form) of
    (headExpr, args)
      | etaAliasArgsMatch (functionFormParamTriples form) args ->
          case collectTyApps headExpr of
            (BackendVarWithIdentity _ mbIdentity targetName, targetTypeArgs) ->
              Just (mbIdentity, targetName, eraseAliasBinderTypeArgs targetTypeArgs)
            _ ->
              Nothing
    _ ->
      Nothing
  where
    binderTypeArgs =
      map functionTypeBinderVar (ffTypeBinders form)
    eraseAliasBinderTypeArgs targetTypeArgs
      | targetTypeArgs == binderTypeArgs = []
      | otherwise = targetTypeArgs

etaAliasArgsMatch :: [(Maybe IdDetails, String, BackendType)] -> [BackendExpr] -> Bool
etaAliasArgsMatch params args =
  length params == length args
    && and (zipWith etaAliasArgMatches params args)

etaAliasArgMatches :: (Maybe IdDetails, String, BackendType) -> BackendExpr -> Bool
etaAliasArgMatches (paramIdentity, paramName, _) arg =
  case backendVarExprRef arg of
    Just argRef -> backendCallableRefMatches (backendCallableRef paramIdentity paramName) argRef
    Nothing -> False

collectValueApps :: BackendExpr -> (BackendExpr, [BackendExpr])
collectValueApps =
  go []
  where
    go args =
      \case
        BackendApp _ fun arg -> go (arg : args) fun
        expr -> (expr, args)

backendVarExprRef :: BackendExpr -> Maybe BackendCallableRef
backendVarExprRef =
  \case
    BackendVarWithIdentity _ mbIdentity name -> Just (backendCallableRef mbIdentity name)
    _ -> Nothing

lowerNonLocalFunctionReference :: ProgramEnv -> ExprEnv -> String -> BackendType -> Maybe IdDetails -> String -> [BackendType] -> LowerM LowerValue
lowerNonLocalFunctionReference env exprEnv context expectedTy mbIdentity name typeArgs =
  case lookupExprEnvValue mbIdentity exprEnv of
    Just value
      | isFunctionLikeBackendType (lvBackendType value) ->
          lowerValueFunctionReference context expectedTy name value typeArgs
    _ ->
      case lookupNonLocalBindingInfo (peBase env) mbIdentity of
        Just binding -> do
          (resolvedTypeArgs, form) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs []
          let actualTy = functionTypeFromForm form
          requireEvidenceFunctionType context (biName binding) expectedTy actualTy
          (functionRef, functionName) <- globalFunctionTarget env context binding resolvedTypeArgs
          pure (functionPointerValueForGlobalTarget expectedTy binding resolvedTypeArgs functionRef (LLVMGlobalRef LLVMPtr functionName))
        Nothing ->
          liftEither (BackendLLVMUnknownFunction name)

lowerValueFunctionReference :: String -> BackendType -> String -> LowerValue -> [BackendType] -> LowerM LowerValue
lowerValueFunctionReference context expectedTy name value typeArgs = do
  actualTy <-
    if null typeArgs
      then pure (lvBackendType value)
      else instantiateCallableTypeM context (lvBackendType value) typeArgs []
  requireEvidenceFunctionType context name expectedTy actualTy
  pure value {lvBackendType = expectedTy}

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
    (\binder body -> BTForallWithIdentity (backendTypeBinderIdentity binder) (backendTypeBinderName binder) (backendTypeBinderBound binder) body)
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
      (BTForallWithIdentity leftIdentity leftName leftBound leftBody, BTForallWithIdentity rightIdentity rightName rightBound rightBody) ->
        runtimeCompatibleMaybeType leftBound rightBound
          && runtimeCompatibleValueType
            (substituteBackendTypeForBinder leftIdentity leftName freshTy leftBody)
            (substituteBackendTypeForBinder rightIdentity rightName freshTy rightBody)
        where
          freshTy =
            BTVarWithIdentity (case leftIdentity of Just {} -> leftIdentity; Nothing -> rightIdentity) freshName

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
      (BTBaseWithIdentity leftIdentity leftBase, BTBaseWithIdentity rightIdentity rightBase) ->
        backendTypeHeadMatches leftIdentity leftBase rightIdentity rightBase
      (BTConWithIdentity leftIdentity leftCon leftArgs, BTConWithIdentity rightIdentity rightCon rightArgs) ->
        backendTypeHeadMatches leftIdentity leftCon rightIdentity rightCon
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

lowerLocalFunctionCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> String -> LocalFunction -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerLocalFunctionCall env callEnv context resultTy _name localFunction typeArgs args = do
  (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (lfForm localFunction) typeArgs args
  let ownerName = lfName localFunction
  let arity = length (ffParams form0)
  case compare (length args) arity of
    GT -> do
      unless (isFunctionLikeBackendType (ffReturnType form0)) $
        liftEither (BackendLLVMArityMismatch ownerName arity (length args))
      let (directArgs, closureArgs) = splitAt arity args
      callee <- lowerLocalFunctionCall env callEnv context (ffReturnType form0) ownerName localFunction typeArgs directArgs
      lowerReturnedFunctionValueCall env callEnv context ownerName resultTy callee closureArgs
    LT ->
      liftEither (BackendLLVMArityMismatch ownerName arity (length args))
    EQ -> do
      bodyEnv <- bindCallArguments env callEnv (lfCapturedEnv localFunction) context ownerName form0 args
      let form = qualifyInstantiatedClosureEntriesWithParamKinds ownerName resolvedTypeArgs (boundParamValueKinds form0 bodyEnv) form0
      lowerExpr env bodyEnv context (ffBody form)
  where
    boundParamValueKinds form bodyEnv =
      foldr bindParam emptyLocalValueKinds (indexed (functionFormParamTriples form))
      where
        bindParam (index0, (mbIdentity, _paramName, paramTy)) =
          bindLocalValueKind
            mbIdentity
            (paramValueKind bodyEnv (ffEvidenceParams form) index0 mbIdentity paramTy)

    paramValueKind bodyEnv evidenceParams index0 mbIdentity paramTy =
      case lookupExprEnvValue mbIdentity bodyEnv of
        Just value -> lvValueKind value
        Nothing
          | Just _ <- lookupExprEnvLocalFunction mbIdentity bodyEnv -> LowerFunctionPointer
          | otherwise -> parameterValueKind evidenceParams index0 paramTy

lowerDirectFunctionCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> FunctionForm -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerDirectFunctionCall env exprEnv context resultTy form0 typeArgs args = do
  (resolvedTypeArgs, form1) <- instantiateFunctionFormWithTypeArgsM context form0 typeArgs args
  let form =
        markFunctionFormEvidenceArgumentsFromValues exprEnv args $
          qualifyInstantiatedClosureEntries "__mlfp_direct_typeapp" resolvedTypeArgs form1
      arity = length (ffParams form)
  case compare (length args) arity of
    GT -> do
      unless (isFunctionLikeBackendType (ffReturnType form)) $
        liftEither (BackendLLVMArityMismatch "lambda" arity (length args))
      let (directArgs, closureArgs) = splitAt arity args
      callee <- lowerSaturatedDirectFunctionCall form directArgs
      lowerReturnedFunctionValueCall env exprEnv context "lambda" resultTy callee closureArgs
    LT ->
      liftEither (BackendLLVMArityMismatch "lambda" arity (length args))
    EQ ->
      lowerSaturatedDirectFunctionCall form args
  where
    lowerSaturatedDirectFunctionCall form2 args0 = do
      bodyEnv <- bindCallArguments env exprEnv exprEnv context "lambda" form2 args0
      lowerExpr env bodyEnv context (ffBody form2)

markFunctionFormEvidenceArgumentsFromValues :: ExprEnv -> [BackendExpr] -> FunctionForm -> FunctionForm
markFunctionFormEvidenceArgumentsFromValues exprEnv args form =
  form
    { ffEvidenceParams =
        ffEvidenceParams form
          `Set.union` Set.fromList
            [ index0
            | (index0, ((_, paramTy), arg)) <- indexed (zip (ffParams form) args),
              isFunctionLikeBackendType paramTy,
              argumentIsFunctionPointer arg
            ]
    }
  where
    argumentIsFunctionPointer arg =
      case collectTyApps arg of
        (BackendVarWithIdentity _ mbIdentity _name, _) ->
          case lookupExprEnvValue mbIdentity exprEnv of
            Just value -> lvValueKind value == LowerFunctionPointer
            Nothing -> False
        _ ->
          False

lowerGlobalCall :: ProgramEnv -> ExprEnv -> String -> BackendType -> Maybe IdDetails -> String -> [BackendType] -> [BackendExpr] -> LowerM LowerValue
lowerGlobalCall env exprEnv context resultTy mbIdentity name typeArgs args =
  case lookupNonLocalBindingInfo (peBase env) mbIdentity of
    Just binding -> do
      (resolvedTypeArgs, form0) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs args
      let bindingName = biName binding
          form = qualifyInstantiatedClosureEntries bindingName resolvedTypeArgs form0
          arity = length (ffParams form)
      case compare (length args) arity of
        GT -> do
          unless (isFunctionLikeBackendType (ffReturnType form)) $
            liftEither (BackendLLVMArityMismatch bindingName arity (length args))
          let (directArgs, closureArgs) = splitAt arity args
          callee <- lowerGlobalCall env exprEnv context (ffReturnType form) (TopLevelId <$> biIdentity binding) bindingName typeArgs directArgs
          lowerReturnedFunctionValueCall env exprEnv context bindingName resultTy callee closureArgs
        LT ->
          liftEither (BackendLLVMArityMismatch name arity (length args))
        EQ ->
          lowerSaturatedGlobalCall binding resolvedTypeArgs form
    Nothing
      | primitiveCallMatches runtimeAndName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          let expectedTypes = [LLVMInt 1, LLVMInt 1]
          zipWithM_ (requireLLVMType context name) expectedTypes callArgs
          result <- emitAssign "call" (LLVMInt 1) (LLVMCall runtimeAndName [(LLVMInt 1, lvOperand arg) | arg <- callArgs])
          pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
    Nothing
      | primitiveCallMatches runtimeStringLengthName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [arg] -> do
              requireLLVMType context name LLVMPtr arg
              result <- emitAssign "call" (LLVMInt 64) (LLVMCall runtimeStringLengthName [(LLVMPtr, lvOperand arg)])
              pure (LowerValue backendIntTy (LLVMInt 64) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringIsEmptyName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [arg] -> do
              requireLLVMType context name LLVMPtr arg
              result <- emitAssign "call" (LLVMInt 1) (LLVMCall runtimeStringIsEmptyName [(LLVMPtr, lvOperand arg)])
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringContainsCharName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, needle] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name (LLVMInt 32) needle
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeStringContainsCharName
                      [(LLVMPtr, lvOperand haystack), (LLVMInt 32, lvOperand needle)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringContainsName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, needle] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name LLVMPtr needle
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeStringContainsName
                      [(LLVMPtr, lvOperand haystack), (LLVMPtr, lvOperand needle)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringEqualsName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [left, right] -> do
              requireLLVMType context name LLVMPtr left
              requireLLVMType context name LLVMPtr right
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeStringEqualsName
                      [(LLVMPtr, lvOperand left), (LLVMPtr, lvOperand right)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringStartsWithName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, prefix] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name LLVMPtr prefix
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeStringStartsWithName
                      [(LLVMPtr, lvOperand haystack), (LLVMPtr, lvOperand prefix)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringEndsWithName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, suffix] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name LLVMPtr suffix
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeStringEndsWithName
                      [(LLVMPtr, lvOperand haystack), (LLVMPtr, lvOperand suffix)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringAppendName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [left, right] -> do
              requireLLVMType context name LLVMPtr left
              requireLLVMType context name LLVMPtr right
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringAppendName
                      [(LLVMPtr, lvOperand left), (LLVMPtr, lvOperand right)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringReplaceCharName -> do
          unless (length args == 3) $
            liftEither (BackendLLVMArityMismatch name 3 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, needle, replacement] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 32) needle
              requireLLVMType context name (LLVMInt 32) replacement
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringReplaceCharName
                      [(LLVMPtr, lvOperand value), (LLVMInt 32, lvOperand needle), (LLVMInt 32, lvOperand replacement)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 3 (length args))
    Nothing
      | primitiveCallMatches runtimeStringReplaceName -> do
          unless (length args == 3) $
            liftEither (BackendLLVMArityMismatch name 3 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, needle, replacement] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name LLVMPtr needle
              requireLLVMType context name LLVMPtr replacement
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringReplaceName
                      [(LLVMPtr, lvOperand haystack), (LLVMPtr, lvOperand needle), (LLVMPtr, lvOperand replacement)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 3 (length args))
    Nothing
      | primitiveCallMatches runtimeStringIndexOfCharName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, needle] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 32) needle
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringIndexOfCharName
                      [(LLVMPtr, lvOperand value), (LLVMInt 32, lvOperand needle)]
                  )
              pure
                ( LowerValue
                    resultTy
                    LLVMPtr
                    result
                    LowerRuntimeValue
                    Nothing
                )
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringIndexOfName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, needle] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name LLVMPtr needle
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringIndexOfName
                      [(LLVMPtr, lvOperand haystack), (LLVMPtr, lvOperand needle)]
                  )
              pure
                ( LowerValue
                    resultTy
                    LLVMPtr
                    result
                    LowerRuntimeValue
                    Nothing
                )
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringSplitName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, delimiter] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name LLVMPtr delimiter
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringSplitName
                      [(LLVMPtr, lvOperand haystack), (LLVMPtr, lvOperand delimiter)]
                  )
              pure
                ( LowerValue
                    resultTy
                    LLVMPtr
                    result
                    LowerRuntimeValue
                    Nothing
                )
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringJoinName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [separator, values] -> do
              requireLLVMType context name LLVMPtr separator
              requireLLVMType context name LLVMPtr values
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringJoinName
                      [(LLVMPtr, lvOperand separator), (LLVMPtr, lvOperand values)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringSplitCharName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [haystack, delimiter] -> do
              requireLLVMType context name LLVMPtr haystack
              requireLLVMType context name (LLVMInt 32) delimiter
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringSplitCharName
                      [(LLVMPtr, lvOperand haystack), (LLVMInt 32, lvOperand delimiter)]
                  )
              pure
                ( LowerValue
                    resultTy
                    LLVMPtr
                    result
                    LowerRuntimeValue
                    Nothing
                )
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringCompareName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [left, right] -> do
              requireLLVMType context name LLVMPtr left
              requireLLVMType context name LLVMPtr right
              result <-
                emitAssign
                  "call"
                  (LLVMInt 64)
                  ( LLVMCall
                      runtimeStringCompareName
                      [(LLVMPtr, lvOperand left), (LLVMPtr, lvOperand right)]
                  )
              pure (LowerValue backendIntTy (LLVMInt 64) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringFromCharName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringFromCharName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringFromIntName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 64) value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringFromIntName
                      [(LLVMInt 64, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringFromBoolName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 1) value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringFromBoolName
                      [(LLVMInt 1, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringFromNatName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name LLVMPtr value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringFromNatName
                      [(LLVMPtr, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringFromListName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name LLVMPtr value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringFromListName
                      [(LLVMPtr, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringToListName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name LLVMPtr value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringToListName
                      [(LLVMPtr, lvOperand value)]
                  )
              pure
                ( LowerValue
                    resultTy
                    LLVMPtr
                    result
                    LowerRuntimeValue
                    Nothing
                )
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringDropName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, count] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 64) count
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringDropName
                      [(LLVMPtr, lvOperand value), (LLVMInt 64, lvOperand count)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringTakeName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, count] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 64) count
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringTakeName
                      [(LLVMPtr, lvOperand value), (LLVMInt 64, lvOperand count)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringSliceName -> do
          unless (length args == 3) $
            liftEither (BackendLLVMArityMismatch name 3 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, start, count] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 64) start
              requireLLVMType context name (LLVMInt 64) count
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringSliceName
                      [(LLVMPtr, lvOperand value), (LLVMInt 64, lvOperand start), (LLVMInt 64, lvOperand count)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 3 (length args))
    Nothing
      | primitiveCallMatches runtimeStringCharAtName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, index] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 64) index
              result <-
                emitAssign
                  "call"
                  (LLVMInt 32)
                  ( LLVMCall
                      runtimeStringCharAtName
                      [(LLVMPtr, lvOperand value), (LLVMInt 64, lvOperand index)]
                  )
              pure (LowerValue backendCharTy (LLVMInt 32) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeStringCharAtOptionName -> do
          unless (length args == 2) $
            liftEither (BackendLLVMArityMismatch name 2 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value, index] -> do
              requireLLVMType context name LLVMPtr value
              requireLLVMType context name (LLVMInt 64) index
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringCharAtOptionName
                      [(LLVMPtr, lvOperand value), (LLVMInt 64, lvOperand index)]
                  )
              pure
                ( LowerValue
                    resultTy
                    LLVMPtr
                    result
                    LowerRuntimeValue
                    Nothing
                )
            _ ->
              liftEither (BackendLLVMArityMismatch name 2 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsDigitName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsDigitName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiLowerName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiLowerName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiUpperName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiUpperName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiAlphaName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiAlphaName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiAlphaNumName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiAlphaNumName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiIdentifierStartName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiIdentifierStartName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiIdentifierContinueName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiIdentifierContinueName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiWhitespaceName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiWhitespaceName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiPunctuationName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiPunctuationName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiPrintableName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiPrintableName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiHexDigitName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiHexDigitName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiLineBreakName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiLineBreakName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharIsAsciiControlName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 1)
                  ( LLVMCall
                      runtimeCharIsAsciiControlName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendBoolTy (LLVMInt 1) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharToAsciiLowerName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 32)
                  ( LLVMCall
                      runtimeCharToAsciiLowerName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendCharTy (LLVMInt 32) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeCharToAsciiUpperName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name (LLVMInt 32) value
              result <-
                emitAssign
                  "call"
                  (LLVMInt 32)
                  ( LLVMCall
                      runtimeCharToAsciiUpperName
                      [(LLVMInt 32, lvOperand value)]
                  )
              pure (LowerValue backendCharTy (LLVMInt 32) result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringToAsciiLowerName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name LLVMPtr value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringToAsciiLowerName
                      [(LLVMPtr, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | primitiveCallMatches runtimeStringToAsciiUpperName -> do
          unless (length args == 1) $
            liftEither (BackendLLVMArityMismatch name 1 (length args))
          callArgs <- traverse (lowerExpr env exprEnv context) args
          case callArgs of
            [value] -> do
              requireLLVMType context name LLVMPtr value
              result <-
                emitAssign
                  "call"
                  LLVMPtr
                  ( LLVMCall
                      runtimeStringToAsciiUpperName
                      [(LLVMPtr, lvOperand value)]
                  )
              pure (LowerValue backendStringTy LLVMPtr result LowerRuntimeValue Nothing)
            _ ->
              liftEither (BackendLLVMArityMismatch name 1 (length args))
    Nothing
      | Just primitiveName <- ioPrimitiveRuntimeName mbIdentity name -> do
          callArgs <- traverse (lowerExpr env exprEnv context) args
          let wrapperName = nativeIOWrapperName primitiveName
          result <- emitAssign "io.call" LLVMPtr (LLVMCall wrapperName [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
          pure (LowerValue resultTy LLVMPtr result LowerRuntimeValue Nothing)
    Nothing ->
      liftEither (BackendLLVMUnknownFunction name)
  where
    primitiveCallMatches expected =
      primitiveRuntimeName mbIdentity name == Just expected

    lowerSaturatedGlobalCall binding resolvedTypeArgs form =
      if requiresInlineCall form
        && Set.member (bindingInfoRef binding) (eeActiveGlobalInlines exprEnv)
        && not (canEmitFunctionForm form)
        then liftEither (BackendLLVMUnsupportedExpression context ("recursive static global " ++ show name))
        else if shouldInlineGlobalCall env exprEnv binding resolvedTypeArgs form args
        then do
          let bodyEnv0 =
                exprEnv
                  { eeActiveGlobalInlines = Set.insert (bindingInfoRef binding) (eeActiveGlobalInlines exprEnv)
                  }
          bodyEnv <- bindCallArguments env exprEnv bodyEnv0 context name form args
          lowerExpr env bodyEnv context (ffBody form)
        else do
          callArgs <- zipWithM (lowerExprForArgument env exprEnv context (ffEvidenceParams form)) (indexed (ffParams form)) args
          bindFunctionArguments env context name form callArgs
          llvmResultTy <- lowerRuntimeValueTypeM env context (ffReturnType form)
          functionName <- globalFunctionName env context binding resolvedTypeArgs
          result <- emitAssign "call" llvmResultTy (LLVMCall functionName [(lvLLVMType arg, lvOperand arg) | arg <- callArgs])
          pure
            ( LowerValue
                (ffReturnType form)
                llvmResultTy
                result
                (functionFormReturnValueKind env form)
                (functionFormReturnConstructedValue env form)
            )

shouldInlineGlobalCall :: ProgramEnv -> ExprEnv -> BindingInfo -> [BackendType] -> FunctionForm -> [BackendExpr] -> Bool
shouldInlineGlobalCall env exprEnv binding resolvedTypeArgs form args =
  ( requiresInlineCall form
      && Set.notMember (bindingInfoRef binding) (eeActiveGlobalInlines exprEnv)
      && not (canEmitRawFunctionPointerReturningForm form)
  )
    || missingPolymorphicSpecialization env binding resolvedTypeArgs
    || any (evidenceArgumentRequiresInline (ffEvidenceParams form) env exprEnv) (zip (indexed (ffParams form)) args)

missingPolymorphicSpecialization :: ProgramEnv -> BindingInfo -> [BackendType] -> Bool
missingPolymorphicSpecialization env binding resolvedTypeArgs =
  not (null (ffTypeBinders (biForm binding)))
    && Map.notMember (specializationKey (specRequestForBinding binding resolvedTypeArgs)) (peSpecializations env)

evidenceArgumentRequiresInline :: Set Int -> ProgramEnv -> ExprEnv -> ((Int, (String, BackendType)), BackendExpr) -> Bool
evidenceArgumentRequiresInline evidenceParams env exprEnv ((index0, (_, paramTy)), arg) =
  isEvidenceArgument evidenceParams index0 paramTy && functionArgumentRequiresInline env exprEnv arg

functionArgumentRequiresInline :: ProgramEnv -> ExprEnv -> BackendExpr -> Bool
functionArgumentRequiresInline env exprEnv arg =
  case collectTyApps arg of
    (BackendVarWithIdentity _ mbIdentity _name, typeArgs)
      | Just localFunction <- lookupExprEnvLocalFunction mbIdentity exprEnv ->
          requiresInlineCall (lfForm localFunction)
      | Just binding <- lookupNonLocalBindingInfo (peBase env) mbIdentity ->
          case instantiateFunctionFormWithTypeArgs "inline evidence argument" (biForm binding) typeArgs [] of
            Right (_, form) -> requiresInlineCall form
            Left _ -> False
    _ -> False

globalFunctionName :: ProgramEnv -> String -> BindingInfo -> [BackendType] -> LowerM String
globalFunctionName env context binding typeArgs =
  snd <$> globalFunctionTarget env context binding typeArgs

globalFunctionTarget :: ProgramEnv -> String -> BindingInfo -> [BackendType] -> LowerM (BackendBindingRef, String)
globalFunctionTarget env context binding typeArgs
  | null (ffTypeBinders (biForm binding)) =
      pure (bindingInfoRef binding, biName binding)
  | otherwise =
      case Map.lookup (specializationKey request) (peSpecializations env) of
        Just specialization -> pure (spBindingRef specialization, spFunctionName specialization)
        Nothing ->
          liftEither (BackendLLVMInternalError ("missing specialization for " ++ biName binding ++ " at " ++ context))
  where
    request = specRequestForBinding binding typeArgs

lowerStaticFunctionArgument :: ProgramEnv -> ExprEnv -> String -> String -> BackendType -> BackendExpr -> LowerM LocalFunction
lowerStaticFunctionArgument env callEnv context paramName expectedTy arg =
  case collectTyApps arg of
    (BackendVarWithIdentity _ mbIdentity _name, typeArgs) ->
      case lookupExprEnvLocalFunction mbIdentity callEnv of
        Just localFunction -> do
          form <- instantiateStaticFunctionForm context expectedTy (lfForm localFunction) typeArgs
          requireStaticFunctionType context paramName expectedTy (localFunction {lfForm = form})
        Nothing ->
          case lookupNonLocalBindingInfo (peBase env) mbIdentity of
            Just binding -> do
              form <- instantiateStaticFunctionForm context expectedTy (biForm binding) typeArgs
              if canEmitReferencedFunctionForm form
                then lowerDirectStaticFunctionArgument callEnv context paramName expectedTy arg
                else
                  requireStaticFunctionType
                    context
                    paramName
                    expectedTy
                    ( LocalFunction
                        { lfName = biName binding,
                          lfForm = form,
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
  case matchTypeParams binderSet Map.empty sourceTy expectedTy >>= resolvedTypeArguments context binders of
    Right typeArgs -> pure typeArgs
    Left err -> liftEither err
  where
    binders = ffTypeBinders form
    binderSet = Set.fromList (map functionTypeBinderKey binders)
    sourceTy = foldr BTArrow (ffReturnType form) (map snd (ffParams form))

lowerDirectStaticFunctionArgument :: ExprEnv -> String -> String -> BackendType -> BackendExpr -> LowerM LocalFunction
lowerDirectStaticFunctionArgument callEnv context paramName expectedTy arg = do
  form <- functionFormFromExpectedM expectedTy arg
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
        { lfName = paramName,
          lfForm = form,
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

bindFunctionArguments :: ProgramEnv -> String -> String -> FunctionForm -> [LowerValue] -> LowerM ()
bindFunctionArguments env context name form args = do
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  expectedTypes <- traverse (\(index0, (_, paramTy)) -> lowerFunctionParameterTypeM env context (ffEvidenceParams form) index0 paramTy) (indexed (ffParams form))
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
  String ->
  FunctionForm ->
  [BackendExpr] ->
  LowerM ExprEnv
bindCallArguments env callEnv bodyEnv0 context name form args = do
  unless (length args == length (ffParams form)) $
    liftEither (BackendLLVMArityMismatch name (length (ffParams form)) (length args))
  foldM bindOne bodyEnv0 (indexed (zip (functionFormParamTriples form) args))
  where
    bindOne bodyEnv (index0, ((mbParamIdentity, paramName, paramTy), arg))
      | isFirstOrderFunctionPointerType paramTy = do
          mbClosureValue <- lowerClosureRuntimeArgumentMaybe env callEnv context paramTy arg
          case mbClosureValue of
            Just value ->
              bindValue bodyEnv mbParamIdentity value
            Nothing
              | firstOrderPointerReference arg ->
                  bindValueFromExpr bodyEnv index0 mbParamIdentity paramName paramTy arg
              | otherwise ->
                  bindStaticFunction bodyEnv mbParamIdentity paramName paramTy arg
      | isInlineFunctionArgument (ffEvidenceParams form) index0 paramTy = do
          bindStaticFunction bodyEnv mbParamIdentity paramName paramTy arg
      | otherwise = do
          bindValueFromExpr bodyEnv index0 mbParamIdentity paramName paramTy arg

    firstOrderPointerReference arg =
      case collectTyApps arg of
        (BackendVarWithIdentity _ mbIdentity _refName, _) ->
          case lookupExprEnvLocalFunction mbIdentity callEnv of
            Just {} -> False
            Nothing -> True
        _ -> False

    bindStaticFunction bodyEnv mbParamIdentity paramName paramTy arg = do
      localFunction <- lowerStaticFunctionArgument env callEnv context paramName paramTy arg
      pure (bindExprEnvLocalFunction mbParamIdentity localFunction bodyEnv)

    bindValueFromExpr bodyEnv index0 mbParamIdentity paramName paramTy arg = do
      value <- lowerExprForArgument env callEnv context (ffEvidenceParams form) (index0, (paramName, paramTy)) arg
      bindValue bodyEnv mbParamIdentity value

    bindValue bodyEnv mbParamIdentity value =
      pure (bindExprEnvValue mbParamIdentity value bodyEnv)

lowerClosureRuntimeArgumentMaybe :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM (Maybe LowerValue)
lowerClosureRuntimeArgumentMaybe env exprEnv context expectedTy arg =
  case closurePointerAliasValue exprEnv arg of
    Just value ->
      pure (Just value {lvBackendType = expectedTy})
    Nothing ->
      case arg of
        BackendClosure _ _ _ _ _
          | alphaEqBackendType expectedTy (backendExprType arg) ->
              Just <$> lowerExpr env exprEnv context arg
        _ ->
          case collectTyApps arg of
            (BackendVarWithIdentity _ mbIdentity _name, typeArgs)
              | Just binding <- lookupNonLocalBindingInfo (peBase env) mbIdentity -> do
                  (_, form) <- instantiateFunctionFormWithTypeArgsM context (biForm binding) typeArgs []
                  if null (ffParams form)
                    && alphaEqBackendType expectedTy (ffReturnType form)
                    && isClosureRuntimeValueType (ffReturnType form)
                    then Just <$> lowerGlobalValue env exprEnv context expectedTy (biName binding) binding typeArgs
                    else pure Nothing
            _ ->
              pure Nothing

isInlineFunctionArgument :: Set Int -> Int -> BackendType -> Bool
isInlineFunctionArgument evidenceParams index0 paramTy =
  isInlineOnlyFunctionParameter evidenceParams index0 paramTy

isInlineOnlyFunctionParameter :: Set Int -> Int -> BackendType -> Bool
isInlineOnlyFunctionParameter evidenceParams index0 paramTy =
  isFunctionLikeBackendType paramTy
    && (evidenceNeedsInlining || nonEvidenceNeedsInlining)
  where
    evidenceLike = isEvidenceArgument evidenceParams index0 paramTy
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
  resolvedTypeArgs <- resolvedTypeArguments context (ffTypeBinders form) substitution
  let substituteTy = substituteBackendTypesByKey substitution
      instantiated =
        FunctionForm
          { ffTypeBinders = [],
            ffParams = [(name, substituteTy ty) | (name, ty) <- ffParams form],
            ffParamIdentities = ffParamIdentities form,
            ffEvidenceParams = ffEvidenceParams form,
            ffBody = substituteExprTypesByKey substitution (ffBody form),
            ffReturnType = substituteTy (ffReturnType form)
          }
  pure (resolvedTypeArgs, instantiated)

resolvedTypeArguments :: String -> [BackendTypeBinder] -> Map BackendTypeSubstitutionKey BackendType -> Either BackendLLVMError [BackendType]
resolvedTypeArguments context binders substitution =
  traverse lookupResolved binders
  where
    lookupResolved binder =
      case Map.lookup (functionTypeBinderKey binder) substitution of
        Just ty -> Right ty
        Nothing -> Left (BackendLLVMInternalError ("missing resolved type argument " ++ show (backendTypeBinderName binder) ++ " at " ++ context))

resolveTypeArguments :: String -> FunctionForm -> [BackendType] -> [BackendExpr] -> Either BackendLLVMError (Map BackendTypeSubstitutionKey BackendType)
resolveTypeArguments context form explicitArgs valueArgs
  | null binders =
      if null explicitArgs
        then Right Map.empty
        else Left (BackendLLVMUnsupportedCall ("unexpected type arguments at " ++ context))
  | length explicitArgs == length binders =
      refineExplicitTypeArguments binders form explicitArgs valueArgs
  | null explicitArgs =
      inferTypeArguments context binders (ffParams form) valueArgs
  | otherwise =
      Left (BackendLLVMUnsupportedCall ("partial type application at " ++ context))
  where
    binders = ffTypeBinders form

refineExplicitTypeArguments ::
  [BackendTypeBinder] ->
  FunctionForm ->
  [BackendType] ->
  [BackendExpr] ->
  Either BackendLLVMError (Map BackendTypeSubstitutionKey BackendType)
refineExplicitTypeArguments binders form explicitArgs [] =
  Right (explicitSubstitutionWithNameAliases binders form explicitArgs)
refineExplicitTypeArguments binders form explicitArgs args = do
  residualSubstitution <-
    foldM
      ( \acc ((_, expectedTy), actualExpr) ->
          matchTypeParams residualBinderSet acc expectedTy (backendExprType actualExpr)
      )
      Map.empty
      (zip instantiatedParams args)
  let refinedExplicitSubstitution =
        Map.map (substituteBackendTypesByKey residualSubstitution) explicitSubstitution
  pure (Map.union refinedExplicitSubstitution residualSubstitution)
  where
    explicitSubstitution =
      explicitSubstitutionWithNameAliases binders form explicitArgs
    instantiatedParams =
      [(name, substituteBackendTypesByKey explicitSubstitution ty) | (name, ty) <- ffParams form]
    residualBinderSet =
      Set.union
        (Set.unions (map freeBackendTypeVarKeys explicitArgs))
        residualParamBinderSet
        `Set.difference` Set.fromList (map functionTypeBinderKey binders)
    residualParamBinderSet =
      Set.fromList
        [ key
        | (_, ty) <- instantiatedParams,
          (key, occurrenceName) <- Set.toList (freeBackendTypeVarOccurrences ty),
          Set.member occurrenceName binderNames
        ]
    binderNames =
      Set.fromList (map backendTypeBinderName binders)

explicitSubstitutionWithNameAliases ::
  [BackendTypeBinder] ->
  FunctionForm ->
  [BackendType] ->
  Map BackendTypeSubstitutionKey BackendType
explicitSubstitutionWithNameAliases binders form explicitArgs =
  Map.union binderSubstitution aliasSubstitution
  where
    binderSubstitution =
      Map.fromList (zip (map functionTypeBinderKey binders) explicitArgs)
    aliasSubstitution =
      Map.fromList
        [ (key, explicitArg)
        | (binder, explicitArg) <- zip binders explicitArgs,
          (key, occurrenceName) <- Set.toList freeParamOccurrences,
          key /= functionTypeBinderKey binder,
          occurrenceName == backendTypeBinderName binder
        ]
    freeParamOccurrences =
      freeFunctionFormTypeVarOccurrences form

freeFunctionFormTypeVarOccurrences :: FunctionForm -> Set (BackendTypeSubstitutionKey, String)
freeFunctionFormTypeVarOccurrences form =
  Set.unions
    [ Set.unions (map (freeBackendTypeVarOccurrences . snd) (ffParams form)),
      freeBackendTypeVarOccurrences (ffReturnType form),
      freeBackendTypeVarOccurrencesInExpr (ffBody form)
    ]

freeBackendTypeVarOccurrences :: BackendType -> Set (BackendTypeSubstitutionKey, String)
freeBackendTypeVarOccurrences =
  \case
    BTVarWithIdentity identity name ->
      Set.singleton (backendTypeSubstitutionKeyFor identity name, name)
    BTArrow dom cod ->
      Set.union (freeBackendTypeVarOccurrences dom) (freeBackendTypeVarOccurrences cod)
    BTBaseWithIdentity {} ->
      Set.empty
    BTConWithIdentity _ _ args ->
      Set.unions (map freeBackendTypeVarOccurrences (NE.toList args))
    BTVarAppWithIdentity identity name args ->
      Set.insert
        (backendTypeSubstitutionKeyFor identity name, name)
        (Set.unions (map freeBackendTypeVarOccurrences (NE.toList args)))
    BTForallWithIdentity identity name mbBound body ->
      Set.union
        (maybe Set.empty freeBackendTypeVarOccurrences mbBound)
        (Set.filter ((/= backendTypeSubstitutionKeyFor identity name) . fst) (freeBackendTypeVarOccurrences body))
    BTMuWithIdentity identity name body ->
      Set.filter ((/= backendTypeSubstitutionKeyFor identity name) . fst) (freeBackendTypeVarOccurrences body)
    BTBottom ->
      Set.empty

freeBackendTypeVarOccurrencesInExpr :: BackendExpr -> Set (BackendTypeSubstitutionKey, String)
freeBackendTypeVarOccurrencesInExpr =
  \case
    BackendVarWithIdentity ty _ _ ->
      freeBackendTypeVarOccurrences ty
    BackendLit ty _ ->
      freeBackendTypeVarOccurrences ty
    BackendLamWithIdentity resultTy _ _ paramTy body ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          freeBackendTypeVarOccurrences paramTy,
          freeBackendTypeVarOccurrencesInExpr body
        ]
    BackendApp resultTy fun arg ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          freeBackendTypeVarOccurrencesInExpr fun,
          freeBackendTypeVarOccurrencesInExpr arg
        ]
    BackendLetWithIdentity resultTy _ _ bindingTy rhs body ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          freeBackendTypeVarOccurrences bindingTy,
          freeBackendTypeVarOccurrencesInExpr rhs,
          freeBackendTypeVarOccurrencesInExpr body
        ]
    BackendTyAbsWithIdentity resultTy identity name mbBound body ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          maybe Set.empty freeBackendTypeVarOccurrences mbBound,
          Set.filter ((/= backendTypeSubstitutionKeyFor identity name) . fst) (freeBackendTypeVarOccurrencesInExpr body)
        ]
    BackendTyApp resultTy fun tyArg ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          freeBackendTypeVarOccurrencesInExpr fun,
          freeBackendTypeVarOccurrences tyArg
        ]
    BackendRoll resultTy payload ->
      Set.union (freeBackendTypeVarOccurrences resultTy) (freeBackendTypeVarOccurrencesInExpr payload)
    BackendUnroll resultTy payload ->
      Set.union (freeBackendTypeVarOccurrences resultTy) (freeBackendTypeVarOccurrencesInExpr payload)
    BackendClosureWithParamIdentities resultTy _ _ captures params body ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          Set.unions (map freeBackendTypeVarOccurrencesInClosureCapture captures),
          Set.unions (map (freeBackendTypeVarOccurrences . backendClosureParamType) params),
          freeBackendTypeVarOccurrencesInExpr body
        ]
    BackendClosureCall resultTy fun args ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          freeBackendTypeVarOccurrencesInExpr fun,
          Set.unions (map freeBackendTypeVarOccurrencesInExpr args)
        ]
    BackendConstructWithIdentity resultTy _ _ args ->
      Set.union (freeBackendTypeVarOccurrences resultTy) (Set.unions (map freeBackendTypeVarOccurrencesInExpr args))
    BackendCase resultTy scrutinee alternatives ->
      Set.unions
        [ freeBackendTypeVarOccurrences resultTy,
          freeBackendTypeVarOccurrencesInExpr scrutinee,
          Set.unions (map freeBackendTypeVarOccurrencesInAlternative (NE.toList alternatives))
        ]

freeBackendTypeVarOccurrencesInClosureCapture :: BackendClosureCapture -> Set (BackendTypeSubstitutionKey, String)
freeBackendTypeVarOccurrencesInClosureCapture capture =
  Set.union
    (freeBackendTypeVarOccurrences (backendClosureCaptureType capture))
    (freeBackendTypeVarOccurrencesInExpr (backendClosureCaptureExpr capture))

freeBackendTypeVarOccurrencesInAlternative :: BackendAlternative -> Set (BackendTypeSubstitutionKey, String)
freeBackendTypeVarOccurrencesInAlternative (BackendAlternative _ body) =
  freeBackendTypeVarOccurrencesInExpr body

inferTypeArguments :: String -> [BackendTypeBinder] -> [(String, BackendType)] -> [BackendExpr] -> Either BackendLLVMError (Map BackendTypeSubstitutionKey BackendType)
inferTypeArguments context binders params args = do
  substitution <-
    foldM
      (\acc ((_, expectedTy), actualExpr) -> matchTypeParams binderSet acc expectedTy (backendExprType actualExpr))
      Map.empty
      (zip params args)
  case filter ((`Map.notMember` substitution) . functionTypeBinderKey) binders of
    [] -> Right substitution
    missing -> Left (BackendLLVMUnsupportedCall ("could not infer type arguments " ++ show (functionTypeBinderNames missing) ++ " at " ++ context))
  where
    binderSet = Set.fromList (map functionTypeBinderKey binders)

data TypeParamMatchStrictness
  = AllowResidualTypeMismatch
  | RejectResidualTypeMismatch
  deriving (Eq, Show)

matchTypeParams :: Set BackendTypeSubstitutionKey -> Map BackendTypeSubstitutionKey BackendType -> BackendType -> BackendType -> Either BackendLLVMError (Map BackendTypeSubstitutionKey BackendType)
matchTypeParams binderSet substitution expected actual =
  matchTypeParamsWith AllowResidualTypeMismatch binderSet substitution expected actual

matchTypeParamsWith ::
  TypeParamMatchStrictness ->
  Set BackendTypeSubstitutionKey ->
  Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendType ->
  Either BackendLLVMError (Map BackendTypeSubstitutionKey BackendType)
matchTypeParamsWith strictness binderSet substitution expected actual =
  case expected of
    BTVarWithIdentity identity name
      | let key = backendTypeSubstitutionKeyFor identity name,
        Set.member key binderSet ->
          case Map.lookup key substitution of
            Nothing -> Right (Map.insert key actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Right substitution
              | otherwise -> Left (BackendLLVMUnsupportedCall ("conflicting inferred type argument for " ++ name))
    _ ->
      case (expected, actual) of
        (BTArrow leftA rightA, BTArrow leftB rightB) ->
          matchTypeParamsWith strictness binderSet substitution leftA leftB >>= \subst ->
            matchTypeParamsWith strictness binderSet subst rightA rightB
        (BTConWithIdentity identityA conA argsA, BTConWithIdentity identityB conB argsB)
          | backendTypeHeadMatches identityA conA identityB conB && length argsA == length argsB ->
              foldM
                (\subst (tyA, tyB) -> matchTypeParamsWith strictness binderSet subst tyA tyB)
                substitution
                (zip (NE.toList argsA) (NE.toList argsB))
        (BTVarAppWithIdentity identity name args, _) ->
          matchTypeParamApplication binderSet substitution (backendTypeSubstitutionKeyFor identity name) name (BTVarWithIdentity identity name) (NE.toList args) actual
        (BTBaseWithIdentity identityA baseA, BTBaseWithIdentity identityB baseB)
          | backendTypeHeadMatches identityA baseA identityB baseB -> Right substitution
        (BTForallWithIdentity identityA nameA boundA bodyA, BTForallWithIdentity identityB nameB boundB bodyB) -> do
          substA <-
            case (boundA, boundB) of
              (Nothing, Nothing) -> Right substitution
              (Just tyA, Just tyB) -> matchTypeParamsWith strictness binderSet substitution tyA tyB
              _ -> Left (BackendLLVMUnsupportedCall "mismatched forall bounds during type argument inference")
          matchTypeParamsWith strictness binderSet substA bodyA (substituteBackendTypeForBinderKey identityB nameB (BTVarWithIdentity identityA nameA) bodyB)
        (BTMuWithIdentity identityA nameA bodyA, BTMuWithIdentity identityB nameB bodyB) ->
          matchTypeParamsWith strictness binderSet substitution bodyA (substituteBackendTypeForBinderKey identityB nameB (BTVarWithIdentity identityA nameA) bodyB)
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
  Set BackendTypeSubstitutionKey ->
  Map BackendTypeSubstitutionKey BackendType ->
  BackendTypeSubstitutionKey ->
  String ->
  BackendType ->
  [BackendType] ->
  BackendType ->
  Either BackendLLVMError (Map BackendTypeSubstitutionKey BackendType)
matchTypeParamApplication binderSet substitution nameKey name expectedHead expectedArgs actual =
  case decomposeBackendTypeHead actual of
    Just (actualHead, actualArgs)
      | length expectedArgs == length actualArgs -> do
          substitution' <-
            if Set.member nameKey binderSet
              then bindTypeParam nameKey actualHead
              else matchRigidHead expectedHead actualHead
          foldM
            (\subst (expectedArg, actualArg) -> matchTypeParamsWith RejectResidualTypeMismatch binderSet subst expectedArg actualArg)
            substitution'
            (zip expectedArgs actualArgs)
      | otherwise ->
          Left (BackendLLVMUnsupportedCall ("type application arity mismatch during type argument inference for " ++ name))
    _ ->
      Left (BackendLLVMUnsupportedCall ("expected applied type while inferring type argument for " ++ name))
  where
    matchRigidHead rigidExpectedHead rigidActualHead
      | typeApplicationHeadMatches rigidExpectedHead rigidActualHead = Right substitution
      | otherwise =
          Left
            ( BackendLLVMUnsupportedCall
                ( "rigid type application head mismatch during type argument inference: expected "
                    ++ show rigidExpectedHead
                    ++ ", got "
                    ++ show rigidActualHead
                )
            )

    bindTypeParam paramName actualHead =
      case Map.lookup paramName substitution of
        Nothing -> Right (Map.insert paramName actualHead substitution)
        Just previous
          | typeApplicationHeadMatches previous actualHead -> Right substitution
          | otherwise -> Left (BackendLLVMUnsupportedCall ("conflicting inferred type argument for " ++ paramNameDisplay))
      where
        paramNameDisplay = backendTypeSubstitutionKeyName paramName

typeApplicationHeadMatches :: BackendType -> BackendType -> Bool
typeApplicationHeadMatches (BTVarWithIdentity leftIdentity leftName) (BTVarWithIdentity rightIdentity rightName) =
  typeBinderRefMatches leftIdentity leftName rightIdentity rightName
typeApplicationHeadMatches left right =
  alphaEqBackendType left right

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

lowerConstruct :: ProgramEnv -> ExprEnv -> String -> BackendType -> Maybe SymbolIdentity -> String -> [BackendExpr] -> LowerM LowerValue
lowerConstruct env exprEnv context resultTy mbIdentity name args =
  case lookupConstructorRuntime (peBase env) mbIdentity name of
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
      object <- emitMalloc env context (constructorObjectBytes (length args))
      tagPtr <- emitGep "tag.ptr" object constructorTagOffset
      emitStore (LLVMInt 64) (LLVMIntLiteral 64 (crTag constructorRuntime)) tagPtr
      zipWithM_ (storeField object) [0 ..] argValues
      resultLLVMType <- lowerBackendTypeM env context resultTy
      unless (resultLLVMType == LLVMPtr) $
        liftEither (BackendLLVMUnsupportedType context resultTy)
      pure
        ( LowerValue
            resultTy
            LLVMPtr
            object
            LowerRuntimeValue
            (Just (constructedValueForConstructorKey (crValueKey constructorRuntime) (map lvValueKind argValues)))
        )
  where
    storeField object index0 value = do
      fieldPtr <- emitGep "field.ptr" object (constructorFieldOffset index0)
      emitStore (lvLLVMType value) (lvOperand value) fieldPtr

lowerConstructField :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> LowerM LowerValue
lowerConstructField env exprEnv context fieldTy arg
  | isFirstOrderFunctionPointerType fieldTy = do
      mbClosureValue <- lowerClosureRuntimeArgumentMaybe env exprEnv context fieldTy arg
      case mbClosureValue of
        Just value -> do
          expectedTy <- lowerRuntimeValueTypeM env context fieldTy
          requireLLVMType context "constructor field" expectedTy value
          pure value {lvBackendType = fieldTy}
        Nothing -> do
          value <- lowerStoredFunctionArgument env exprEnv context fieldTy arg
          let (paramTys, returnTy) = collectArrowsType fieldTy
          lowerReturnedPartialClosureValue env exprEnv context fieldTy value paramTys returnTy []
  | isClosureRuntimeValueType fieldTy = do
      value <- lowerExpr env exprEnv context arg
      expectedTy <- lowerRuntimeValueTypeM env context fieldTy
      requireLLVMType context "constructor field" expectedTy value
      pure value {lvBackendType = fieldTy}
  | otherwise =
      lowerExpr env exprEnv context arg

lowerCase :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> NonEmpty BackendAlternative -> LowerM LowerValue
lowerCase env exprEnv context resultTy scrutinee alternatives =
  case scrutinee of
    BackendConstructWithIdentity {backendExprType = scrutineeTy, backendConstructIdentity = mbIdentity, backendConstructName = name, backendConstructArgs = args} ->
      case lookupConstructorRuntime (peBase env) mbIdentity name of
        Just constructorRuntime -> do
          fieldTys <- constructorFieldTypesForScrutinee env context constructorRuntime scrutineeTy
          if any backendTypeRequiresStaticSpecialization fieldTys
            then lowerImmediateConstructCase env exprEnv context resultTy mbIdentity name args fieldTys alternatives
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
  tagPtr <- emitGep "case.tag.ptr" (lvOperand scrutineeValue) constructorTagOffset
  tagValue <- emitAssign "case.tag" (LLVMInt 64) (LLVMLoad (LLVMInt 64) tagPtr)
  altLabels <- traverse (const (freshBlock "case.alt")) (NE.toList alternatives)
  defaultLabel <- maybe (freshBlock "case.default") pure (lookupDefaultLabel altLabels)
  joinLabel <- freshBlock "case.join"
  let constructorTargets = mapMaybe constructorSwitchTarget (zip (NE.toList alternatives) altLabels)
      switchTargets = [(tag, label) | (tag, label, _) <- constructorTargets]
      resultValueKind = combineValueKinds resultTy (map (alternativeBodyValueKind scrutineeValue) alternativesList)
  rejectDuplicateSwitchTargets constructorTargets
  finishCurrentBlock (LLVMSwitch (LLVMInt 64) tagValue defaultLabel switchTargets)
  incoming <- concat <$> zipWithM (lowerAlternative resultValueKind resultLLVMType joinLabel scrutineeValue) (NE.toList alternatives) altLabels
  when (lookupDefaultLabel altLabels == Nothing) $ do
    startBlock defaultLabel
    finishCurrentBlock LLVMUnreachable
  startBlock joinLabel
  result <- emitAssign "case.result" resultLLVMType (LLVMPhi resultLLVMType [(operand, label) | (operand, label, _, _) <- incoming])
  pure
    ( LowerValue
        resultTy
        resultLLVMType
        result
        resultValueKind
        (mergeConstructedValues [constructed | (_, _, _, constructed) <- incoming])
    )
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
        BackendConstructorPatternWithBinderIdentities mbIdentity name _ ->
          case lookupConstructorRuntime (peBase env) mbIdentity name of
            Just constructorRuntime -> Just (crTag constructorRuntime, label, name)
            Nothing -> Nothing

    rejectDuplicateSwitchTargets targets =
      case firstDuplicate (map (\(tag, _, _) -> tag) targets) of
        Just tag ->
          liftEither (BackendLLVMUnsupportedExpression context ("duplicate constructor case tag " ++ show tag))
        Nothing ->
          pure ()

    lowerAlternative resultValueKind resultLLVMType joinLabel scrutineeValue alternative label = do
      startBlock label
      exprEnv' <- bindAlternativePattern scrutineeValue alternative
      rawBodyValue <- lowerExpr env exprEnv' context (backendAltBody alternative)
      bodyValue <- normalizeCaseResultValue resultValueKind rawBodyValue
      unless (lvLLVMType bodyValue == resultLLVMType) $
        liftEither (BackendLLVMInternalError ("case alternative type mismatch at " ++ context))
      sourceLabel <- gets fsCurrentLabel
      finishCurrentBlock (LLVMBr joinLabel)
      pure [(lvOperand bodyValue, sourceLabel, lvValueKind bodyValue, lvConstructedValue bodyValue)]

    normalizeCaseResultValue targetKind value
      | targetKind == LowerClosureRecord,
        lvValueKind value == LowerFunctionPointer,
        isFirstOrderFunctionPointerType (lvBackendType value) =
          let (paramTys, returnTy) = collectArrowsType (lvBackendType value)
           in lowerReturnedPartialClosureValue env exprEnv (context ++ " case result") resultTy value paramTys returnTy []
      | otherwise =
          pure value

    alternativeBodyValueKind scrutineeValue alternative =
      backendExprValueKindWith env Set.empty (alternativeValueKinds scrutineeValue alternative) (backendAltBody alternative)

    alternativeValueKinds scrutineeValue (BackendAlternative pattern0 _) =
      case pattern0 of
        BackendDefaultPattern ->
          exprEnvLocalValueKinds exprEnv
        BackendConstructorPatternWithBinderIdentities mbIdentity constructorName binders ->
          patternBinderValueKinds scrutineeValue mbIdentity constructorName binders `unionLocalValueKinds` exprEnvLocalValueKinds exprEnv

    patternBinderValueKinds scrutineeValue mbIdentity constructorName binders =
      foldr bindPatternField emptyLocalValueKinds (zip [0 :: Int ..] (zip binders fieldTys))
      where
        mbConstructorRuntime =
          lookupConstructorRuntime (peBase env) mbIdentity constructorName
        mbConstructorValueKey =
          crValueKey <$> mbConstructorRuntime
        fieldTys =
          fromMaybe [] $
            mbConstructorRuntime >>= \constructorRuntime ->
              constructorRuntimeFieldTypes constructorRuntime (lvBackendType scrutineeValue)
        binderFieldValueKind index0 fieldTy =
          fromMaybe (constructorFieldStoredValueKind fieldTy) $
            mbConstructorValueKey >>= \constructorKey ->
              lvConstructedValue scrutineeValue >>= constructedFieldValueKindByKey constructorKey index0
        bindPatternField (index0, (binder, fieldTy)) =
          bindLocalValueKind
            (backendPatternBinderIdentity binder)
            (binderFieldValueKind index0 fieldTy)

    bindAlternativePattern scrutineeValue (BackendAlternative pattern0 body) =
      case pattern0 of
        BackendDefaultPattern ->
          pure exprEnv
        BackendConstructorPatternWithBinderIdentities mbIdentity name binders ->
          case lookupConstructorRuntime (peBase env) mbIdentity name of
            Nothing ->
              liftEither (BackendLLVMUnknownConstructor name)
            Just constructorRuntime -> do
              let constructorKey = crValueKey constructorRuntime
                  binderNames = map backendPatternBinderName binders
              fieldTys <- constructorFieldTypesForScrutinee env context constructorRuntime (lvBackendType scrutineeValue)
              unless (length fieldTys == length binderNames) $
                liftEither (BackendLLVMArityMismatch name (length fieldTys) (length binderNames))
              let usedBinders = freeTermVars body
                  bodyBinderTypes = backendExprVarTypesFor (termBoundKeyRefs [backendPatternBinderIdentity binder | binder <- binders]) body
                  effectiveFieldTys =
                    [ patternBinderBodyType bodyBinderTypes binder fieldTy
                      | (binder, fieldTy) <- zip binders fieldTys
                    ]
              loadedFields <- mapMaybe id <$> traverse (loadUsedField constructorKey usedBinders scrutineeValue) (zip3 [0 :: Int ..] binders effectiveFieldTys)
              pure (foldr bindLoadedField exprEnv loadedFields)

    bindLoadedField (identity, _name, value) acc =
      bindExprEnvValue identity value acc

    patternBinderBodyType bodyBinderTypes binder fieldTy =
      case backendPatternBinderIdentity binder >>= lowerLocalKey of
        Just key ->
          Map.findWithDefault fieldTy (TermBoundIdentity key) bodyBinderTypes
        Nothing ->
          fieldTy

    loadUsedField constructorKey usedBinders scrutineeValue (index0, binder, fieldTy)
      | patternBinderUsedBy usedBinders binder = do
          loaded <- loadField constructorKey scrutineeValue index0 fieldTy
          pure (Just (backendPatternBinderIdentity binder, binderName, loaded))
      | otherwise =
          pure Nothing
      where
        binderName = backendPatternBinderName binder

    loadField constructorKey scrutineeValue index0 fieldTy = do
      llvmTy <- lowerStoredFieldTypeM env context fieldTy
      fieldPtr <- emitGep "case.field.ptr" (lvOperand scrutineeValue) (constructorFieldOffset index0)
      loaded <- emitAssign "case.field" llvmTy (LLVMLoad llvmTy fieldPtr)
      pure (LowerValue fieldTy llvmTy loaded (fieldValueKind constructorKey scrutineeValue index0 fieldTy) Nothing)

    fieldValueKind constructorKey scrutineeValue index0 fieldTy =
      case lvConstructedValue scrutineeValue of
        Just constructed
          | Just kind <- constructedFieldValueKindByKey constructorKey index0 constructed ->
              kind
        _ ->
          constructorFieldStoredValueKind fieldTy

constructorFieldStoredValueKind :: BackendType -> LowerValueKind
constructorFieldStoredValueKind fieldTy
  | isFunctionLikeBackendType fieldTy = LowerClosureRecord
  | otherwise = LowerRuntimeValue

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

valueKindForType :: BackendType -> LowerValueKind
valueKindForType ty
  | isFirstOrderFunctionPointerType ty = LowerFunctionPointer
  | isClosureRuntimeValueType ty = LowerClosureRecord
  | otherwise = LowerRuntimeValue

functionFormParamValueKinds :: FunctionForm -> LocalValueKinds
functionFormParamValueKinds form =
  foldr bindParam emptyLocalValueKinds (indexed (functionFormParamTriples form))
  where
    bindParam (index0, (mbIdentity, _paramName, paramTy)) =
      bindLocalValueKind
        mbIdentity
        (parameterValueKind (ffEvidenceParams form) index0 paramTy)

functionFormReturnValueKind :: ProgramEnv -> FunctionForm -> LowerValueKind
functionFormReturnValueKind env form =
  functionFormReturnValueKindWith env Set.empty form

functionFormReturnValueKindWith :: ProgramEnv -> Set BackendBindingRef -> FunctionForm -> LowerValueKind
functionFormReturnValueKindWith env visitedGlobals form =
  backendExprValueKindWith env visitedGlobals (functionFormParamValueKinds form) (ffBody form)

functionFormReturnConstructedValue :: ProgramEnv -> FunctionForm -> Maybe ConstructedValue
functionFormReturnConstructedValue env form =
  backendExprConstructedValueWith env Set.empty (functionFormParamValueKinds form) Map.empty (ffBody form)

backendExprValueKindWith :: ProgramEnv -> Set BackendBindingRef -> LocalValueKinds -> BackendExpr -> LowerValueKind
backendExprValueKindWith env visitedGlobals valueKinds expr
  | not (isFunctionLikeBackendType (backendExprType expr)) = LowerRuntimeValue
  | otherwise =
      case expr of
        BackendVarWithIdentity ty mbIdentity name ->
          variableValueKind ty mbIdentity name []
        BackendTyApp _ fun _ ->
          case collectTyApps expr of
            (BackendVarWithIdentity ty mbIdentity name, typeArgs) ->
              variableValueKind ty mbIdentity name typeArgs
            _ ->
              backendExprValueKindWith env visitedGlobals valueKinds fun
        BackendLetWithIdentity _ mbIdentity _name bindingTy rhs body ->
          backendExprValueKindWith env visitedGlobals valueKindsForBody body
          where
            valueKindsForBody =
              case functionLikeAliasValueKind valueKinds rhs of
                Just kind ->
                  bindLocalValueKind mbIdentity kind valueKinds
                Nothing ->
                  case functionFormFromExpected bindingTy rhs of
                    form
                      | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
                          deleteLocalValueKind mbIdentity valueKinds
                    _ ->
                      bindLocalValueKind mbIdentity (backendExprValueKindWith env visitedGlobals valueKinds rhs) valueKinds
        BackendCase _ scrutinee alternatives ->
          combineValueKinds
            (backendExprType expr)
            [ backendExprValueKindWith env visitedGlobals (alternativeValueKinds scrutinee alternative) (backendAltBody alternative)
            | alternative <- NE.toList alternatives
            ]
        BackendClosure _ _ _ _ _ ->
          LowerClosureRecord
        _ ->
          valueKindForType (backendExprType expr)
  where
    variableValueKind ty mbIdentity name typeArgs =
      variableValueKindWith valueKinds ty mbIdentity name typeArgs

    alternativeValueKinds scrutinee alternative =
      patternValueKinds (backendExprType scrutinee) (backendAltPattern alternative) `unionLocalValueKinds` valueKinds

    patternValueKinds scrutineeTy =
      \case
        BackendDefaultPattern ->
          emptyLocalValueKinds
        BackendConstructorPatternWithBinderIdentities mbIdentity constructorName binders ->
          foldr bindPatternField emptyLocalValueKinds (zip binders fieldTys)
          where
            fieldTys =
              fromMaybe [] $
                lookupConstructorRuntime (peBase env) mbIdentity constructorName >>= \constructorRuntime ->
                  constructorRuntimeFieldTypes constructorRuntime scrutineeTy
            bindPatternField (binder, fieldTy) =
              bindLocalValueKind
                (backendPatternBinderIdentity binder)
                (constructorFieldStoredValueKind fieldTy)

    functionLikeAliasValueKind kinds =
      \case
        BackendVarWithIdentity ty mbIdentity name
          | isFunctionLikeBackendType ty ->
              Just (variableValueKindWith kinds ty mbIdentity name [])
        expr0@(BackendTyApp ty fun _)
          | isFunctionLikeBackendType ty ->
              case collectTyApps expr0 of
                (BackendVarWithIdentity varTy mbIdentity name, typeArgs) ->
                  Just (variableValueKindWith kinds varTy mbIdentity name typeArgs)
                _ ->
                  functionLikeAliasValueKind kinds fun
        BackendLetWithIdentity ty mbIdentity _name bindingTy rhs body
          | isFunctionLikeBackendType ty ->
              let kindsForBody =
                    case functionLikeAliasValueKind kinds rhs of
                      Just kind ->
                        bindLocalValueKind mbIdentity kind kinds
                      Nothing ->
                        case functionFormFromExpected bindingTy rhs of
                          form
                            | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
                                deleteLocalValueKind mbIdentity kinds
                          _ ->
                            bindLocalValueKind mbIdentity (backendExprValueKindWith env visitedGlobals kinds rhs) kinds
               in functionLikeAliasValueKind kindsForBody body
        _ ->
          Nothing

    variableValueKindWith kinds ty mbIdentity _name typeArgs =
      case lookupLocalValueKind mbIdentity kinds of
        Just kind ->
          kind
        Nothing ->
          case lookupNonLocalBindingInfo (peBase env) mbIdentity of
            Just binding ->
              case instantiateFunctionFormWithTypeArgs "value-kind classification" (biForm binding) typeArgs [] of
                Right (_, form)
                  | not (null (ffParams form)) ->
                      LowerFunctionPointer
                  | Set.member (bindingInfoRef binding) visitedGlobals ->
                      valueKindForType ty
                  | otherwise ->
                      functionFormReturnValueKindWith env (Set.insert (bindingInfoRef binding) visitedGlobals) form
                _ ->
                  LowerFunctionPointer
            Nothing ->
              valueKindForType ty

backendExprConstructedValueWith ::
  ProgramEnv ->
  Set BackendBindingRef ->
  LocalValueKinds ->
  LocalConstructedValues ->
  BackendExpr ->
  Maybe ConstructedValue
backendExprConstructedValueWith env visitedGlobals valueKinds constructedValues =
  \case
    BackendVarWithIdentity _ mbIdentity _name ->
      variableConstructedValue mbIdentity []
    expr@(BackendTyApp _ fun _) ->
      case collectTyApps expr of
        (BackendVarWithIdentity _ mbIdentity _name, typeArgs) ->
          variableConstructedValue mbIdentity typeArgs
        _ ->
          backendExprConstructedValueWith env visitedGlobals valueKinds constructedValues fun
    BackendLetWithIdentity _ mbIdentity _name bindingTy rhs body ->
      backendExprConstructedValueWith env visitedGlobals valueKindsForBody constructedValuesForBody body
      where
        valueKindsForBody =
          case functionLikeAliasValueKind valueKinds rhs of
            Just kind ->
              bindLocalValueKind mbIdentity kind valueKinds
            Nothing ->
              case functionFormFromExpected bindingTy rhs of
                form
                  | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
                      deleteLocalValueKind mbIdentity valueKinds
                _ ->
                  bindLocalValueKind mbIdentity (backendExprValueKindWith env visitedGlobals valueKinds rhs) valueKinds
        constructedValuesForBody =
          case backendExprConstructedValueWith env visitedGlobals valueKinds constructedValues rhs of
            Just constructed ->
              bindLocalConstructedValue mbIdentity constructed constructedValues
            Nothing ->
              deleteLocalConstructedValue mbIdentity constructedValues
    BackendConstructWithIdentity resultTy mbIdentity name args ->
      constructedValueForConstructorKey . crValueKey <$> mbConstructorRuntime <*> pure fieldKinds
      where
        mbConstructorRuntime =
          lookupConstructorRuntime (peBase env) mbIdentity name
        fieldTys =
          fromMaybe (map backendExprType args) $
            mbConstructorRuntime >>= \constructorRuntime ->
              constructorRuntimeFieldTypes constructorRuntime resultTy
        fieldKinds =
          zipWith constructFieldValueKind fieldTys args
        constructFieldValueKind fieldTy _ =
          constructorFieldStoredValueKind fieldTy
    BackendCase _ scrutinee alternatives ->
      mergeConstructedValues
        [ backendExprConstructedValueWith env visitedGlobals (alternativeValueKinds scrutinee alternative) constructedValues (backendAltBody alternative)
        | alternative <- NE.toList alternatives
        ]
    BackendRoll _ payload ->
      backendExprConstructedValueWith env visitedGlobals valueKinds constructedValues payload
    BackendUnroll _ payload ->
      backendExprConstructedValueWith env visitedGlobals valueKinds constructedValues payload
    _ ->
      Nothing
  where
    functionLikeAliasValueKind kinds =
      \case
        BackendVarWithIdentity ty mbIdentity name
          | isFunctionLikeBackendType ty ->
              Just (variableValueKindWith kinds ty mbIdentity name [])
        expr@(BackendTyApp ty fun _)
          | isFunctionLikeBackendType ty ->
              case collectTyApps expr of
                (BackendVarWithIdentity varTy mbIdentity name, typeArgs) ->
                  Just (variableValueKindWith kinds varTy mbIdentity name typeArgs)
                _ ->
                  functionLikeAliasValueKind kinds fun
        BackendLetWithIdentity ty mbIdentity _name bindingTy rhs body
          | isFunctionLikeBackendType ty ->
              let kindsForBody =
                    case functionLikeAliasValueKind kinds rhs of
                      Just kind ->
                        bindLocalValueKind mbIdentity kind kinds
                      Nothing ->
                        case functionFormFromExpected bindingTy rhs of
                          form
                            | not (null (ffTypeBinders form)) || not (null (ffParams form)) ->
                                deleteLocalValueKind mbIdentity kinds
                          _ ->
                            bindLocalValueKind mbIdentity (backendExprValueKindWith env visitedGlobals kinds rhs) kinds
               in functionLikeAliasValueKind kindsForBody body
        _ ->
          Nothing

    alternativeValueKinds scrutinee alternative =
      patternValueKinds (backendExprType scrutinee) (backendAltPattern alternative) `unionLocalValueKinds` valueKinds

    patternValueKinds scrutineeTy =
      \case
        BackendDefaultPattern ->
          emptyLocalValueKinds
        BackendConstructorPatternWithBinderIdentities mbIdentity constructorName binders ->
          foldr bindPatternField emptyLocalValueKinds (zip binders fieldTys)
          where
            fieldTys =
              fromMaybe [] $
                lookupConstructorRuntime (peBase env) mbIdentity constructorName >>= \constructorRuntime ->
                  constructorRuntimeFieldTypes constructorRuntime scrutineeTy
            bindPatternField (binder, fieldTy) =
              bindLocalValueKind
                (backendPatternBinderIdentity binder)
                (constructorFieldStoredValueKind fieldTy)

    variableValueKindWith kinds ty mbIdentity _name typeArgs =
      case lookupLocalValueKind mbIdentity kinds of
        Just kind ->
          kind
        Nothing ->
          case lookupNonLocalBindingInfo (peBase env) mbIdentity of
            Just binding ->
              case instantiateFunctionFormWithTypeArgs "value-kind classification" (biForm binding) typeArgs [] of
                Right (_, form)
                  | not (null (ffParams form)) ->
                      LowerFunctionPointer
                  | Set.member (bindingInfoRef binding) visitedGlobals ->
                      valueKindForType ty
                  | otherwise ->
                      functionFormReturnValueKindWith env (Set.insert (bindingInfoRef binding) visitedGlobals) form
                _ ->
                  LowerFunctionPointer
            Nothing ->
              valueKindForType ty

    variableConstructedValue mbIdentity typeArgs =
      case lookupLocalConstructedValue mbIdentity constructedValues of
        Just constructed ->
          Just constructed
        Nothing ->
          case lookupNonLocalBindingInfo (peBase env) mbIdentity of
            Just binding ->
              case instantiateFunctionFormWithTypeArgs "constructed-value classification" (biForm binding) typeArgs [] of
                Right (_, form)
                  | null (ffParams form),
                    Set.notMember (bindingInfoRef binding) visitedGlobals ->
                      backendExprConstructedValueWith
                        env
                        (Set.insert (bindingInfoRef binding) visitedGlobals)
                        (functionFormParamValueKinds form)
                        Map.empty
                        (ffBody form)
                _ ->
                  Nothing
            Nothing ->
              Nothing

parameterValueKind :: Set Int -> Int -> BackendType -> LowerValueKind
parameterValueKind evidenceParams index0 ty
  | isEvidenceParameter evidenceParams index0 ty = LowerFunctionPointer
  | isFirstOrderFunctionPointerType ty = LowerFunctionPointer
  | isClosureRuntimeValueType ty = LowerClosureRecord
  | otherwise = LowerRuntimeValue

localFunctionParameterValueKind :: Set Int -> Int -> BackendType -> LowerValueKind
localFunctionParameterValueKind evidenceParams index0 ty
  | isEvidenceParameter evidenceParams index0 ty = LowerFunctionPointer
  | isFirstOrderFunctionPointerType ty = LowerFunctionPointer
  | isClosureRuntimeValueType ty = LowerClosureRecord
  | otherwise = LowerRuntimeValue

lowerValueForType :: BackendType -> LLVMType -> LLVMOperand -> LowerValue
lowerValueForType ty llvmTy operand =
  LowerValue ty llvmTy operand (valueKindForType ty) Nothing

functionPointerValue :: BackendType -> LLVMOperand -> LowerValue
functionPointerValue ty operand =
  LowerValue ty LLVMPtr operand LowerFunctionPointer Nothing

functionPointerValueForBindingRef :: BackendType -> BackendBindingRef -> LLVMOperand -> LowerValue
functionPointerValueForBindingRef ty ref operand =
  (functionPointerValue ty operand) {lvBindingRef = Just ref}

functionPointerValueForGlobalTarget :: BackendType -> BindingInfo -> [BackendType] -> BackendBindingRef -> LLVMOperand -> LowerValue
functionPointerValueForGlobalTarget ty binding resolvedTypeArgs ref operand =
  (functionPointerValueForBindingRef ty ref operand) {lvSymbolIdentity = mbIdentity}
  where
    mbIdentity
      | null resolvedTypeArgs = biIdentity binding
      | otherwise = Nothing

lowerImmediateConstructCase ::
  ProgramEnv ->
  ExprEnv ->
  String ->
  BackendType ->
  Maybe SymbolIdentity ->
  String ->
  [BackendExpr] ->
  [BackendType] ->
  NonEmpty BackendAlternative ->
  LowerM LowerValue
lowerImmediateConstructCase env exprEnv context resultTy mbConstructorIdentity constructorName args fieldTys alternatives = do
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
      case [alternative | alternative@(BackendAlternative pattern0 _) <- alternativesList, patternMatchesConstructor pattern0] of
        alternative : _ -> Just alternative
        [] ->
          case [alternative | alternative@(BackendAlternative BackendDefaultPattern _) <- alternativesList] of
            alternative : _ -> Just alternative
            [] -> Nothing

    patternMatchesConstructor =
      \case
        BackendConstructorPatternWithBinderIdentities mbPatternIdentity name _ ->
          constructorPatternMatches mbConstructorIdentity constructorName mbPatternIdentity name
        BackendDefaultPattern ->
          False

    constructorPatternMatches (Just constructorIdentity) _ (Just patternIdentity) _ =
      constructorIdentity == patternIdentity
    constructorPatternMatches (Just {}) _ Nothing _ =
      False
    constructorPatternMatches _ expectedName _ patternName =
      patternName == expectedName

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
      case firstDuplicate [constructorPatternDuplicateKey mbIdentity name | BackendAlternative (BackendConstructorPatternWithBinderIdentities mbIdentity name _) _ <- alternativesList] of
        Just key ->
          liftEither (BackendLLVMUnsupportedExpression context ("duplicate constructor case alternative " ++ show key))
        Nothing ->
          pure ()

    constructorPatternDuplicateKey (Just identity) _ =
      Left identity
    constructorPatternDuplicateKey Nothing name =
      Right name

    lowerUnmatchedImmediateCase = do
      zipWithM_ evaluateUnusedField fieldTys args
      expectedTy <- lowerCaseResultTypeM env context resultTy
      finishCurrentBlock LLVMUnreachable
      continuationLabel <- freshBlock "case.unreachable.cont"
      startBlock continuationLabel
      operand <- dummyOperandAfterUnreachable context expectedTy
      pure (LowerValue resultTy expectedTy operand (valueKindForType resultTy) Nothing)

    bindImmediateAlternativePattern (BackendAlternative pattern0 body) =
      case pattern0 of
        BackendDefaultPattern -> do
          zipWithM_ evaluateUnusedField fieldTys args
          pure exprEnv
        BackendConstructorPatternWithBinderIdentities mbPatternIdentity name binders -> do
          let binderNames = map backendPatternBinderName binders
          unless (constructorPatternMatches mbConstructorIdentity constructorName mbPatternIdentity name) $
            liftEither
              ( BackendLLVMUnsupportedExpression
                  context
                  ("selected immediate constructor mismatch " ++ show name ++ " for " ++ show constructorName)
              )
          unless (length binderNames == length fieldTys) $
            liftEither (BackendLLVMArityMismatch name (length fieldTys) (length binderNames))
          foldM
            (bindUsedField (freeTermVars body))
            exprEnv
            (zip3 binders fieldTys args)

    bindUsedField usedBinders acc (binder, fieldTy, arg)
      | backendTypeRequiresStaticSpecialization fieldTy = do
          localFunction <- lowerStaticFunctionArgument env exprEnv context binderName fieldTy arg
          if patternBinderUsedBy usedBinders binder
            then pure (bindExprEnvLocalFunction binderIdentity localFunction acc)
            else pure acc
      | backendTypeHasRuntimeRepresentation env fieldTy = do
        value <- lowerConstructField env exprEnv context fieldTy arg
        expectedTy <- lowerRuntimeValueTypeM env context fieldTy
        requireLLVMType context constructorName expectedTy value
        if patternBinderUsedBy usedBinders binder
          then pure (bindExprEnvValue binderIdentity value acc)
          else pure acc
      | otherwise = do
          liftEither (BackendLLVMUnsupportedType ("field " ++ show binderName ++ " at " ++ context) fieldTy)
      where
        binderIdentity = backendPatternBinderIdentity binder
        binderName = backendPatternBinderName binder

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
  case Structural.matchFocusedStructuralConstructor Map.empty (crData constructorRuntime) constructor Map.empty scrutineeTy of
    Right structuralMatch ->
      Just (Structural.srcmFieldTypes structuralMatch)
    Left _ ->
      case Structural.matchConstructorResult (backendDataParameterRefs (crData constructorRuntime)) parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
        Just substitution ->
          Just (map (substituteBackendTypesByKey substitution) (backendConstructorFields constructor))
        Nothing ->
          Nothing
  where
    constructor = crConstructor constructorRuntime
    parameters =
      Set.fromList
        ( backendDataParameterKeys (crData constructorRuntime)
            ++ [ backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder)
                 | binder <- backendConstructorForalls constructor
               ]
        )

lowerRollLike :: ProgramEnv -> ExprEnv -> String -> BackendType -> BackendExpr -> String -> LowerM LowerValue
lowerRollLike env exprEnv context resultTy payload nodeName = do
  payloadValue <- lowerExpr env exprEnv context payload
  resultLLVMType <- lowerBackendTypeM env context resultTy
  if resultLLVMType == lvLLVMType payloadValue
    then pure payloadValue {lvBackendType = resultTy, lvLLVMType = resultLLVMType}
    else liftEither (BackendLLVMUnsupportedExpression context ("representation-changing " ++ nodeName))

lowerBackendTypeM :: ProgramEnv -> String -> BackendType -> LowerM LLVMType
lowerBackendTypeM env context ty =
  case lowerBackendType env context ty of
    Right llvmTy -> pure llvmTy
    Left err -> liftEither err

lowerBackendType :: ProgramEnv -> String -> BackendType -> Either BackendLLVMError LLVMType
lowerBackendType env context ty =
  case ty of
    BTBaseWithIdentity identity base@(BaseTy name)
      | backendBuiltinHeadMatches "Int" identity base -> Right (LLVMInt 64)
      | backendBuiltinHeadMatches "Bool" identity base -> Right (LLVMInt 1)
      | backendBuiltinHeadMatches "Char" identity base -> Right (LLVMInt 32)
      | backendBuiltinHeadMatches "String" identity base -> Right LLVMPtr
      | backendBuiltinHeadMatches ioTypeName identity base -> Right LLVMPtr
      | maybe False (const True) (lookupDataRuntimeByHead (peBase env) identity name) -> Right LLVMPtr
      | otherwise -> Left (BackendLLVMUnsupportedType context ty)
    BTConWithIdentity identity base@(BaseTy name) _
      | backendBuiltinHeadMatches ioTypeName identity base -> Right LLVMPtr
      | maybe False (const True) (lookupDataRuntimeByHead (peBase env) identity name) -> Right LLVMPtr
      | otherwise -> Left (BackendLLVMUnsupportedType context ty)
    BTBase (BaseTy name)
      | maybe False (const True) (lookupDataRuntimeByHead (peBase env) Nothing name) -> Right LLVMPtr
      | otherwise -> Left (BackendLLVMUnsupportedType context ty)
    BTCon (BaseTy name) _
      | maybe False (const True) (lookupDataRuntimeByHead (peBase env) Nothing name) -> Right LLVMPtr
      | otherwise -> Left (BackendLLVMUnsupportedType context ty)
    BTMu {} -> Right LLVMPtr
    BTVar {} -> Left (BackendLLVMUnsupportedType context ty)
    BTVarApp {} -> Left (BackendLLVMUnsupportedType context ty)
    BTArrow {} -> Right LLVMPtr
    BTForall {} -> Left (BackendLLVMUnsupportedType context ty)
    BTBottom -> Left (BackendLLVMUnsupportedType context ty)

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVarWithIdentity identity name -> Just (BTVarWithIdentity identity name, [])
    BTBaseWithIdentity identity name -> Just (BTBaseWithIdentity identity name, [])
    BTConWithIdentity identity name args -> Just (BTBaseWithIdentity identity name, NE.toList args)
    BTVarAppWithIdentity identity name args -> Just (BTVarWithIdentity identity name, NE.toList args)
    _ -> Nothing

emitMalloc :: ProgramEnv -> String -> Int -> LowerM LLVMOperand
emitMalloc env context size
  | not (runtimeBindingNameAvailable (peBase env) runtimeMallocName) =
      liftEither (BackendLLVMUnsupportedExpression context ("reserved runtime binding " ++ show runtimeMallocName))
  | otherwise =
      emitAssign "malloc" LLVMPtr (LLVMCall runtimeMallocName [(LLVMInt 64, LLVMIntLiteral 64 (toInteger size))])

substituteExprTypesByKey :: Map BackendTypeSubstitutionKey BackendType -> BackendExpr -> BackendExpr
substituteExprTypesByKey substitution =
  go substitution
  where
    go subst =
      let substituteTy = substituteBackendTypesByKey subst
       in
      \case
        BackendVarWithIdentity resultTy mbIdentity name ->
          BackendVarWithIdentity (substituteTy resultTy) mbIdentity name
        BackendLit resultTy lit ->
          BackendLit (substituteTy resultTy) lit
        BackendLamWithIdentity resultTy mbIdentity name paramTy body ->
          BackendLamWithIdentity (substituteTy resultTy) mbIdentity name (substituteTy paramTy) (go subst body)
        BackendApp resultTy fun arg ->
          BackendApp (substituteTy resultTy) (go subst fun) (go subst arg)
        BackendLetWithIdentity resultTy mbIdentity name bindingTy rhs body ->
          BackendLetWithIdentity (substituteTy resultTy) mbIdentity name (substituteTy bindingTy) (go subst rhs) (go subst body)
        BackendTyAbsWithIdentity resultTy identity name mbBound body ->
          BackendTyAbsWithIdentity
            (substituteTy resultTy)
            identity
            name
            (fmap substituteTy mbBound)
            (go (deleteTypeBinderSubstitution identity name subst) body)
        BackendTyApp resultTy fun argTy ->
          BackendTyApp (substituteTy resultTy) (go subst fun) (substituteTy argTy)
        BackendConstructWithIdentity resultTy mbIdentity name args ->
          BackendConstructWithIdentity (substituteTy resultTy) mbIdentity name (map (go subst) args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase (substituteTy resultTy) (go subst scrutinee) (fmap (substituteAlternative subst) alternatives)
        BackendRoll resultTy payload ->
          BackendRoll (substituteTy resultTy) (go subst payload)
        BackendUnroll resultTy payload ->
          BackendUnroll (substituteTy resultTy) (go subst payload)
        BackendClosureWithParamIdentities resultTy entryIdentity entryName captures params body ->
          BackendClosureWithParamIdentities
            (substituteTy resultTy)
            entryIdentity
            entryName
            (map (substituteCapture subst) captures)
            [param {backendClosureParamType = substituteTy (backendClosureParamType param)} | param <- params]
            (go subst body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall (substituteTy resultTy) (go subst fun) (map (go subst) args)

    substituteAlternative subst alternative =
      alternative {backendAltBody = go subst (backendAltBody alternative)}

    substituteCapture subst capture =
      let substituteTy = substituteBackendTypesByKey subst
       in
      capture
        { backendClosureCaptureType = substituteTy (backendClosureCaptureType capture),
          backendClosureCaptureExpr = go subst (backendClosureCaptureExpr capture)
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
